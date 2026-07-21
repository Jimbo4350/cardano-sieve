# Kupo Rewrite: Pattern ADT & Storage Model — Design Notes and Next Steps

# Status

📜 Proposed 2026-07-17

# Context

Companion to [ADR-020](./Kupo-rewrite-ADR-020-indexing-architecture.md). This
records the outcome of a design session on two questions ADR-020 left open:
what the `Pattern` ADT should look like, and how matched data should be laid out
on disk. It is a decision-record-plus-next-steps, not a frozen ADR.

The design lens throughout is the project's performance priority (see the
benchmark goal): **tier 1 = query latency + fast syncing, tier 2 = memory
usage.** Disk footprint is not a stated priority. **Sieve targets full feature
parity with kupo and wins on performance** — same feature surface, measurably
better numbers; it is not trying to out-feature kupo.

# What kupo actually does today (verified against master, not the issue tracker)

Product research into kupo's issue tracker suggested that spend-side data and a
non-SQLite backend were unmet gaps. Reading the actual code (both the
IntersectMBO fork and CardanoSolutions master, byte-identical `src/`) corrected
the picture — the issue tracker both lags and, in one case, *overstates* the
code. Recording the true state so we neither chase already-solved problems nor
inherit phantom ones:

- **kupo already captures spend provenance.** v2.10 added two columns to the
  match row: `spent_by` (the output reference of the input that consumed the
  UTxO — i.e. spending transaction id + input index) and `spent_with` (the
  **redeemer**, as raw binary data). So current kupo *can* answer "which tx
  spent this, and with what redeemer" — which means DEX cancel-vs-fill is
  already answerable. What kupo does *not* store is the full script context
  (datums/other inputs of the spending tx); only the redeemer. It writes these
  columns by **UPDATE-ing the existing row** on spend.
- **kupo is effectively SQLite-only — the "PostgreSQL backend" is a phantom.**
  `Kupo/App/Database/Postgres.hs` looks like a second backend but is a
  byte-for-byte copy of the SQLite module that *still opens SQLite connections*.
  `--postgres-url` exists only in an off-by-default `-Dpostgres` build and even
  then runs SQLite. So pluggable storage is *not* actually shipped. This removes
  any backend tension: a single engine is both the parity baseline and the
  performance-optimal choice.
- **kupo builds its query indexes lazily.** Indexes are created at runtime via
  `installIndex` (e.g. `inputsBySpentAt`, `policiesByPolicyId`), not in the
  schema migrations, and some are even created on-the-fly per query and dropped
  after (`withTemporaryIndex`). This confirms deferred index building is an
  accepted, shipped approach.

## kupo's six tables (current schema)

- **`inputs`** — the match table (misleadingly named; it holds matched
  *outputs*, not tx inputs). One row per matched UTxO: `output_reference`
  (PK, plus virtual `output_index` / `transaction_index` slices),
  `address` (+ virtual `payment_credential` = last 56 chars), `value` (raw
  multi-asset blob), `datum_info` (+ virtual `datum_hash`), `script_hash`,
  `created_at` (slot), `spent_at` (slot, NULL ⇒ unspent), `spent_by`,
  `spent_with`. This single table folds creation, the live/unspent flag, and
  spend provenance together.
- **`policies`** — `(output_reference, policy_id)`, FK to `inputs` with
  `ON DELETE CASCADE`. The output→policy index that backs policy/asset
  matching (asset-name precision comes from filtering the `value` blob).
- **`binary_data`** — `binary_data_hash → binary_data`. Shared, de-duplicated
  datum-preimage store, referenced by hash from `inputs.datum_hash`.
- **`scripts`** — `script_hash → script`. Shared, de-duplicated script store,
  referenced by hash.
- **`patterns`** — the persisted set of active patterns (survives restart,
  managed at runtime over the HTTP API).
- **`checkpoints`** — `(header_hash, slot_no)`. Processed-block points for
  resumption and the basis for rollback rewind.

# Decisions

1. **Priorities as above.** Every trade below is read through tier-1 query
   latency + fast sync, tier-2 memory.

2. **Mirror kupo's match dimensions; do not expand them.** The research found
   no unmet demand for new match *shapes* (no value-threshold, datum-content,
   regex, script-hash, or wall-clock matching). A lean ADT ≈ kupo's set also
   minimises per-output work and decode surface, which serves fast sync. The
   differentiation is in the data model and performance, not the match set.

3. **Rollback: keep handling, drop the diff feature.** Rewind-on-fork stays
   (mandatory for correctness — the index is wrong without it). The "tell me
   which UTxOs changed since slot X" feature (which forces retaining orphaned
   rows, i.e. write/storage cost) is explicitly out of scope.

4. **Model the spend as a separate, append-only event — not columns on the
   output row.** Capability-wise this matches kupo's `spent_by`/`spent_with`;
   the divergence is *how* it is stored and *why*:
   - kupo writes spend data by UPDATE-ing the (large, growing) match row.
   - Sieve records the spend as an INSERT into a separate table, keyed by the
     consumed output reference, carrying the spending tx id + slot (always,
     cheap) and the redeemer / script context (opt-in, since redeemers are the
     heavy bytes). This keeps the big historical table append-only and avoids
     update-with-row-growth on the sync hot path.
   - No new `Pattern` constructors are needed: patterns still match at output
     *creation*; spends are captured automatically for already-tracked outputs.
     The expensive half (checking each tx input against tracked outputs) is work
     kupo already does — the only delta is the bytes written.

5. **Three-table model for the match data** (replacing kupo's single `inputs`):
   see the storage section below. Core idea: **"unspent" is a set you delete
   from, not a flag you update.**

6. **Defer index building during initial sync.** Accepted trade for fast
   catch-up; queries may be slower until indexes are built. This is idiomatic —
   kupo does exactly this. (Revisit only if research surfaces a common mode
   where clients hammer queries *during* catch-up.)

# Storage model: sieve's tables and how kupo's map in

Sieve's three data tables replace **only** kupo's `inputs` table. Kupo's other
five tables carry over largely unchanged.

- **`outputs`** — append-only, one row per output ever created (full data).
  Never mutated. Serves history / replay. ⇐ kupo `inputs.created_at` side.
- **`unspent`** — the live UTxO set. INSERT on create, DELETE-by-PK on spend.
  Indexed by match dimension (address, payment credential, policy, asset).
  Because it only ever *contains* unspent UTxOs, "unspent at address X" is a
  plain index scan — no `spent_at IS NULL` predicate, no anti-join, no scanning
  spent rows. It stays small and cache-resident (memory win). ⇐ this is the
  live subset of kupo `inputs`.
- **`spends`** — append-only spend provenance. INSERT on spend: consumed output
  reference, spending tx id, slot, and (opt-in) redeemer. ⇐ kupo
  `inputs.spent_at/spent_by/spent_with`.

Why the DELETE on `unspent` is not the write cost we avoided: it is a
delete-by-primary-key on a *small* table (no row growth, cheap index upkeep),
whereas kupo's `spent_at` UPDATE mutates a large, growing row. This split also
resolves kupo issue #206 by construction: it gives the small-table query speed
that kupo only reaches with `--prune-utxo` (which deletes spent rows once past
the ~36h stability window, keeping only the live set — fast, but *loses all
history*), **while still keeping full history** in the append-only `outputs`
table. Fast unspent reads and full history at the same time.

Carried over from kupo, tied in as follows:

- **`patterns`** — persisted active patterns. Orthogonal to the three data
  tables. Likely extended with a per-pattern start-point (see open questions).
- **`checkpoints`** — resumption points + rollback rewind basis. Orthogonal;
  mandatory given we keep rollback handling.
- **`binary_data`** / **`scripts`** — shared, de-duplicated preimage stores,
  referenced by hash from `outputs` (and by `spends` if we store spending
  redeemers there). Carry over unchanged.
- **`policies`** (and any asset-name index) — the join table that backs
  policy/asset matching. This is the one piece coupled to the `unspent`
  decision: to serve "unspent UTxOs with policy X" cheaply, the policy index
  must hang off `unspent` and cascade-delete on spend (rather than off
  `outputs` and be filtered). Resolve alongside the fat-vs-thin question.

# Pattern-query coverage and where the indexes live

Checked strictly from a "what can be queried" standpoint: the schema serves
every kupo pattern-query dimension and more — with one gap.

| Pattern dimension            | Served by                          | Covered |
|------------------------------|------------------------------------|---------|
| exact address                | `address` column + index           | yes     |
| payment credential           | derived column + index             | yes     |
| delegation / stake part      | derived column + index             | yes     |
| payment + delegation         | composite                          | yes     |
| transaction id               | `output_reference` (txid slice)    | yes     |
| output reference             | primary-key lookup                 | yes     |
| policy id                    | `policies` join table              | yes     |
| asset id (policy.name)       | `policies` + filter on `value`     | yes     |
| wildcard                     | slot-range scan                    | yes     |
| metadata tag                 | ingest-only selector (no storage)  | yes (*) |
| filter: spent / unspent      | `unspent` vs `outputs` + `spends`  | yes (+) |
| filter: created/spent slot   | `created_at` / spend-slot index    | yes     |

(*) `MatchMetadataTag` needs no storage. Verified against kupo's code: kupo
never stores transaction metadata. The tag is an **ingest-time selector** —
during block matching, if a transaction carries the tag, its outputs are
indexed as ordinary rows; the metadata itself is discarded. It is *not*
queryable after the fact (there is no `GET /matches/{tag}`), so there is
nothing to index or serve from our tables. The only requirement it places on
the pipeline is that the matcher can see each transaction's metadata at decode
time — a decode concern, not a storage one. So the three-table model covers
**every** kupo pattern-query dimension. (Serving `GET /metadata/{slot}` is a
separate matter: kupo re-fetches the block from the upstream node on demand via
a second connection — again, no storage. Out of scope for the schema.)

## The core structural trade-off: where the match-dimension indexes live

Every pattern query above is ultimately an index lookup on address /
credential / policy / slot. The whole schema decision reduces to *which table
carries those indexes*:

- **Kupo's way — one `outputs` table, `spent_at` flag, indexes on the big
  table; unspent served by a partial index (`WHERE spent_at IS NULL`).**
  Pro: one index set → least write-amplification → best for sync; simplest.
  Con: the indexes sit on a table that grows unbounded with history → larger,
  more page-cache pressure (worse tier-2 memory); unspent lookups probe a huge
  index; spend is an UPDATE on a growing row.

- **The split — put the match indexes on the small `unspent` table, keep
  `outputs` append-only and lightly indexed.**
  Pro: index maintenance scales with the *small* live set, not all history →
  fast unspent latency + low memory; `outputs` stays a cheap append-only log
  (few indexes → good sync on the bulk insert).
  Con: two inserts per output instead of one; the `policies` index must also
  hang off `unspent` and cascade-delete on spend; and spent-inclusive /
  historical queries are slower (few indexes on `outputs`) unless we pay to
  index it too.

The split wins *for these priorities specifically* because it moves the
expensive indexes onto the small hot table. The price is (a) an extra insert
per output and (b) cold historical queries. Given tier-1 = unspent latency +
sync and tier-2 = memory, and that spent/historical queries are the cold path,
that trade is worth taking.

**Resolution (supersedes Q1):** thin-but-covering `unspent` — it carries the
match keys and all the query indexes; `outputs` is append-only and minimally
indexed (PK, `created_at`, txid slice); `spends` is append-only. Index the
small table, not the big one. Historical / spent-inclusive queries are
best-effort (scan, or an opt-in index on `outputs`), not first-class-fast.

# Storage schema (Phase 3)

Realised in `Cardano.Sieve.Schema` (authoritative DDL — `createSchema` +
`installDeferredIndexes`), as inline `CREATE TABLE IF NOT EXISTS` string
literals matching the Phase-1 convention. No migration engine yet:
wipe-and-resync during development. The Phase-1 placeholder `block_header` table
is superseded by these tables and is removed from the ingest path when Phase 4
repoints ingest. Verified valid against `sqlite3` (foreign-key graph sound).

Tables (columns are `BLOB` unless noted; `?` = nullable):

- **blocks** (`slot_no` INT PK, `header_hash`) — durable slot → header-hash for
  `created_at`/`spent_at` in results; one row per block with matched activity,
  never pruned (sparse `checkpoints` can't serve this).
- **outputs** (`output_reference` PK, `transaction_id`, `address`, `value`,
  `datum_hash?`, `script_hash?`, `created_slot` INT) — append-only history,
  never mutated, primary-key-only.
- **unspent** (as `outputs` plus `payment_credential?`, `delegation_credential?`)
  — the live set; INSERT on create, DELETE by PK on spend; carries every query
  index.
- **spends** (`output_reference` PK, `spending_transaction_id`,
  `spending_input_index` INT, `spent_slot` INT, `redeemer?`) — append-only
  provenance.
- **policies** (`output_reference`, `policy_id`, PK both, FK → `outputs`) —
  policy/asset index over full history; unspent-by-policy joins to `unspent` by
  PK.
- **binary_data** (`datum_hash` PK, `datum`), **scripts** (`script_hash` PK,
  `script`) — deduplicated preimages.
- **patterns** (`selector` TEXT PK) — active selectors (text form coupled to
  Phase 8). **checkpoints** (`slot_no` INT PK, `header_hash`) — sparse pruned
  resume points.

Deferred indexes (installed post-sync), all on the small tables: `unspent` by
`address` / `payment_credential` / `delegation_credential` / `transaction_id` /
`created_slot`; `policies(policy_id)`; `spends(spent_slot)`. `outputs` stays
primary-key-only.

Micro-decisions settled during Phase 3: (1) a `blocks` table rather than a
per-row header hash — narrower hot table (tier-2 memory) and a reliable hash
source; (2) `value` carried in `unspent` so the common query (unspent UTxOs by
address/credential/policy) is join-free; (3) real
`payment_credential`/`delegation_credential` columns populated at insert, not
kupo-style virtual/generated columns; (4) `policies` → `outputs` (append-only,
INSERT-only) so historical policy/asset queries stay indexed, at the cost of a
PK join on the unspent-by-policy path.

# Open questions

**Q1 — How fat is the `unspent` row? — RESOLVED.** Thin-but-covering: the
`unspent` row carries the columns we filter and commonly return (address,
value, datum hash) plus all the query indexes, and PK-joins back to `outputs`
only for rare heavy fields (datum preimage, script). See "Pattern-query
coverage and where the indexes live" above for the reasoning, and "Storage
schema (Phase 3)" for the realised tables (note the `policies` index was later
pointed at `outputs`, not `unspent`, so historical policy queries stay indexed).

**Q2 — `Pattern` constructor set — RESOLVED by the parity goal.** Mirror
kupo's full set: `MatchAny` (with the bootstrap toggle for `*` vs `*/*`),
`MatchExact`, `MatchPayment`, `MatchDelegation`, `MatchPaymentAndDelegation`,
`MatchTransactionId`, `MatchOutputReference`, `MatchPolicyId`, `MatchAssetId`,
`MatchMetadataTag`. Each is a type case *and* a demand on the decode path /
indexes. `MatchMetadataTag` is the odd one out: it is ingest-only (a decode
cost — the matcher must see each tx's metadata — but no storage and no query
index), per the coverage section above.

**Q3 — Metadata serving — RESOLVED (no storage).** Verified: kupo does not
store metadata at all. `MatchMetadataTag` is ingest-only (see coverage), and
`GET /metadata/{slot}` re-fetches the block from the upstream node on demand via
a dedicated second connection. So there is no metadata-content indexing to
design — parity means replicating the on-demand refetch path, which is a
networking/decode subsystem, not a schema concern. (Note: read-only replicas
can't serve it, since they have no upstream to refetch from.)

**Q4 — Per-pattern backfill / start-point.** When a pattern is added at runtime,
can we index it from an arbitrary past slot *without* a full resync or a
whole-DB rollback (kupo's current `PUT /patterns` is capped to the ~k safe
zone)? This is a genuine divergence: it means each pattern carries its own
start-point and we can backfill one pattern in isolation. Design not yet done.

**Q5 — Storage backend strategy — RESOLVED (single engine).** Kupo's
"PostgreSQL backend" is a non-functional stub (see above); kupo is SQLite-only
in practice. So parity requires exactly one engine — which is also the
performance-optimal choice. Commit to a single embedded engine tuned hard for
our priorities; a second backend is explicitly out of scope (it would *exceed*
kupo, not match it).

**Q6 — The surface pattern language.** Design the textual syntax (kupo's
`patternFromText` equivalent) as a *projection onto* the frozen ADT — the ADT is
the source of truth, the syntax is one way to construct it. Deliberately last.

# Next steps (ordered)

1. ~~**Freeze the `Pattern` ADT**~~ — DONE as `Cardano.Sieve.Selector`
   (Phase 1) and the pure matcher `Cardano.Sieve.Satisfies.satisfies`
   (Phase 2, tested).
2. ~~**Design the `outputs` / `unspent` / `spends` schema**~~ — DONE as
   `Cardano.Sieve.Schema` (Phase 3); Q1 resolved (thin-but-covering), policy
   index over `outputs`. See "Storage schema (Phase 3)" above.
3. **Decode stage (Phase 4)** — build `OutputContext` (+ the fields the schema
   needs for persistence) from decoded blocks; repoint ingest at the Phase-3
   tables and retire the `block_header` placeholder. This is ADR-020's
   targeted-extraction vs full-decode tension.
4. **Design the parser** (Q6) as a projection onto the frozen ADT.
5. **Park explicitly as future:** metadata serving/refetch (Q3), per-pattern
   backfill (Q4), and prune/GC around the stability window.
