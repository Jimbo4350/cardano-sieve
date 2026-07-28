# Query parity with kupo

A tracking checklist: every kind of lookup kupo can answer, how cardano-sieve
answers the same thing, and whether we're confident it's fast. The project goal
is to serve **the same queries as kupo, at least as fast** — this page is how we
keep score.

No SQLite knowledge assumed. If you already know databases well, skip to
[The checklist](#the-checklist).

---

## Background in one minute

- A **UTxO** is an unspent transaction output — coins/tokens sitting at an
  address, waiting to be spent. kupo and sieve are **chain-indexes**: they follow
  the blockchain and keep a queryable database of UTxOs, so apps can ask "what's
  at this address?" without scanning the whole chain themselves.
- Clients ask questions like *"the unspent UTxOs at this address, newest first."*
  Our job is to answer quickly.
- Speed comes from **indexes**. Think of the index at the back of a book: instead
  of reading every page to find a topic, you look it up and jump straight to it.
  Without an index the database reads **every row** — a "full scan" — which is
  slow and gets slower as the data grows.
- One wrinkle drives most of the decisions here: clients want results **newest
  first, one page at a time**. An index that helps *find* the matching rows does
  not automatically help *order* them — the database may still do a separate
  **sorting step**. An index built on *both* the thing you filter by *and* the
  thing you sort by (the creation slot — a slot is a blockchain timestamp) removes
  that sorting step and lets the database stop after one page. Those are the
  **composite** (two-column) indexes below.

**Words you'll see in benchmark output** (`./bench/run-query-bench.sh`):

| phrase | meaning |
|---|---|
| `SCAN <table>` | read every row — no index was used (slow) |
| `SEARCH <table> USING INDEX x` | jumped straight to matches via index `x` (good) |
| `USE TEMP B-TREE FOR ORDER BY` | an extra sorting step — avoidable with the right composite index |

---

## What kupo lets clients ask

A kupo query has two parts:

1. **A pattern** — *what* to match. One of:
   `*` (everything), an exact address, a payment credential, a stake/delegation
   credential, both credentials together, a transaction id, a single output, a
   policy id, or a policy + asset name.
2. **Optional filters** — refine the result: unspent-only vs spent-included,
   a slot (time) range, ordering (newest/oldest first), and a few extra
   narrowers (by policy, asset, transaction, output index).

sieve has to answer all of these to reach parity. (One kupo pattern, the
metadata tag `{tag}`, is *not* a query — it only chooses what to index — so it's
out of scope here.)

---

## The checklist

Status key:
**✅ covered & fast** · **🟡 works but slow** · **⛔ gap — not handled yet** ·
**🧊 cold path — works but slow *by design*** (full history isn't indexed; see
notes).

| The client asks… | kupo request | How sieve answers it | Index used | Status |
|---|---|---|---|---|
| Everything, newest page | `GET /matches/*?unspent` | read the live-set table in slot order | `unspent(created_slot)` | ✅ |
| UTxOs at an address | `GET /matches/{address}?unspent` | filter by address, already slot-ordered | `unspent(address, created_slot)` | ✅ |
| UTxOs under a payment key | `GET /matches/{cred}/*?unspent` | filter by payment credential | `unspent(payment_credential, created_slot)` | ✅ |
| UTxOs for a stake key | `GET /matches/*/{cred}?unspent` | filter by delegation credential | `unspent(delegation_credential, created_slot)` | ✅ |
| A specific base address (both keys) | `GET /matches/{cred}/{cred}?unspent` | filter on both credentials | `unspent(payment_credential, delegation_credential, created_slot)` | ✅ |
| Outputs of one transaction | `GET /matches/*@{txid}?unspent` | filter by transaction id | `unspent(transaction_id, created_slot)` | ✅ |
| One specific output | `GET /matches/{ix}@{txid}?unspent` | primary-key lookup | primary key | ✅ |
| UTxOs holding a policy's tokens | `GET /matches/{policy}.*?unspent` | join the policy index to the live set | `policies(policy_id, created_slot)` | ✅ |
| UTxOs holding one specific asset | `GET /matches/{policy}.{name}?unspent` | join on policy + asset name (now stored) | `policies(policy_id, asset_name, created_slot)` | ✅ |
| …restricted to a time window | `?created_after=…&created_before=…` | slot range; the composites already carry the slot | composite 2nd column / `unspent(created_slot)` | ✅ |
| …oldest first instead | `?order=oldest_first` | read the same index the other direction | same index | ✅ |
| Include spent (full history) | `?spent` or no flag | read the full-history `outputs` table | — (outputs is unindexed) | 🧊 cold path |
| Also return datum/script bodies | `?resolve_hashes` | look up the body by its hash | primary key | ✅ |
| Fetch a datum / script by hash | `GET /datums/{h}`, `/scripts/{h}` | direct lookup | primary key | ✅ |
| List checkpoints (sync points) | `GET /checkpoints` | direct lookup | primary key | ✅ |

---

## Known gaps and slow paths

As of 2026-07-28 every *queryable* shape is index-backed and sub-millisecond on
the preview database. The earlier gaps (payment+delegation, asset name) and the
slow policy path were closed by adding the composite indexes and widening the
`policies` table to carry `asset_name` + `created_slot`. One deliberate cold path
remains:

1. **🧊 Spent / full-history queries (slow by design).** "Unspent" queries hit a
   small, fully-indexed live-set table (fast). "Include spent" queries hit the
   full-history `outputs` table, which we deliberately leave unindexed so writing
   new history stays cheap (a sync-speed priority). These work but scan
   (~380 ms). Acceptable unless a spent-history query turns out to be hot.

Two caveats (not gaps):
- **Policy / asset queries still read the full-history `policies` table.** The
  composite indexes make them fast because the newest matches for an active
  policy are mostly unspent, so the join finds a page quickly. A pathological
  policy whose recent entries are mostly *spent* would make the join walk more
  rows. The escalation — a live-set `unspent_policies` table — is documented next
  to the `policies` table in `src/Cardano/Sieve/Schema.hs`, gated on the kupo
  comparison.
- **Widening `policies`** to one row per *(output, policy, asset)* grew that
  table ~1.6× (≈1.85M → ≈3.0M rows on this dataset) — added ingest write work, a
  tier-1 sync cost still to be confirmed with the sync benchmark.

---

## What's proven vs. what's still inferred

**Proven (measured — `bench/run-query-bench.sh`, 2026-07-28, ~387k-UTxO preview
database):** *every* queryable shape is now index-backed. The composites remove
the newest-first sort (`USE TEMP B-TREE FOR ORDER BY` disappears from every
indexed plan). Representative medians, with the index vs without:

| shape | busiest key | no index | with index |
|---|---|---|---|
| by address | 155k matches | ~120 ms | **~0 ms** |
| payment+delegation | 1.9k | ~47 ms | **~0 ms** |
| policy id | 1.2M policy rows | ~550 ms | **~0 ms** |
| asset id | 223k | ~346 ms | **~0 ms** |
| wildcard, recent page | all 387k | ~470 ms | **~0 ms** |
| spent-inclusive (`outputs`) | 155k | ~380 ms | (cold path, unindexed) |

**Still inferred (not yet proven):** we've only measured sieve **against
itself**. "Parity" isn't proven until the *same* queries run against a real kupo
and sieve meets or beats it on each row.

**Next (the real parity gate):** run this query set against a kupo instance and
record kupo's numbers beside sieve's — that comparison decides whether the
policy/asset live-set redesign is ever needed, and it pairs with a sync-benchmark
run to confirm the widened-`policies` ingest cost is acceptable.

---

## Running the benchmark yourself

```bash
./bench/run-query-bench.sh
```

It handles the database for you (syncs one the first time, reuses it after) and
prints, per query: what it does, the kupo endpoint it stands in for, the exact
SQL, and the latency with vs. without its index. See the comments at the top of
that script for a short SQLite primer.

Related notes: `Kupo-rewrite-pattern-and-storage-design.md` (why the tables are
shaped the way they are) and the `sieve-query-index-tuning` engineering memo (the
raw index decision).
