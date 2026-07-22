# Kupo Feature-Parity Checklist

# Status

📜 Reference 2026-07-17

# Context

Companion to
[Kupo-rewrite-pattern-and-storage-design](./Kupo-rewrite-pattern-and-storage-design.md).
A code-verified inventory of kupo's externally-observable feature surface, so
"feature parity" is a testable list rather than a vibe. Built by reading the
source at `~/repos/kupo` (IntersectMBO fork, current nightly — CHANGELOG shows
`2.12.0` UNRELEASED, last release `2.11.0` 2025-06-10). `~/repos/kupo-master`
(CardanoSolutions) has a byte-identical `src/`, so this applies to both. All
`file:line` citations are relative to the kupo checkout.

**Three findings to flag up front:**

1. **The PostgreSQL backend does not exist as a real backend.**
   `Kupo/App/Database/Postgres.hs:11-14` is a byte-for-byte copy of the SQLite
   module that *still opens SQLite connections*. `--postgres-url` only appears
   in a `-Dpostgres` build (a manual cabal flag, default off —
   `kupo.cabal:41-44`), and even then the engine is SQLite. Treat SQLite as the
   only real backend.
2. **`--max-concurrency` does not exist.** It is referenced only in the text of
   the `503` error message (`Data/Http/Error.hs:294`); there is no such flag.
3. **Metadata is never stored** (see §4). The default HTTP port in code is
   **1442** (`Options.hs:262`), despite a stale comment saying 1337.

# 1. CLI flags / options

Source: `src/Kupo/Options.hs`, types in `src/Kupo/Data/Configuration.hs`.

Top-level commands (`Command`, `Options.hs:109-114`): `Run` (default), `copy`,
`health-check`, `version`.

## Chain-producer selection (mutually exclusive, `Options.hs:172-188`)

| Flag | Arg | Description |
|---|---|---|
| `--node-socket` | FILEPATH | cardano-node local socket (with `--node-config`) |
| `--node-config` | FILEPATH | node config file (for network params) |
| `--ogmios-host` | IPv4 | Ogmios host (with `--ogmios-port`) |
| `--ogmios-port` | TCP/PORT | Ogmios port |
| `--hydra-host` | IPv4 | Hydra-node host (with `--hydra-port`) |
| `--hydra-port` | TCP/PORT | Hydra-node port |
| `--read-only` | flag | read-only replica; no network, cannot serve `/metadata` |

## Database location (mutually exclusive, `Options.hs:206-236`)

| Flag | Arg | Description |
|---|---|---|
| `--workdir` | DIRECTORY | on-disk DB dir (`<dir>/kupo.sqlite3`) |
| `--in-memory` | flag | fully in-memory, lost on exit |
| `--postgres-url` | URL | only in `-Dpostgres` build; **still SQLite** |

## Server / sync / storage

| Flag | Arg | Default | Description |
|---|---|---|---|
| `--host` | IPv4 | `127.0.0.1` | HTTP bind address |
| `--port` | TCP/PORT | `1442` | HTTP port |
| `--since` | POINT | — | start point: `origin` / `tip` / `SLOT.HEADERHASH`. Mandatory on first start |
| `--until` | POINT\|SLOT | — | inclusive stop-indexing point (keeps serving queries) |
| `--match` | PATTERN | — | pattern to index; repeatable (logical OR) |
| `--prune-utxo` | flag | off (mark) | remove spent inputs instead of marking them |
| `--gc-interval` | SECONDS | `3600` | background GC/pruning interval |
| `--defer-db-indexes` | flag | install | skip non-essential indexes until next restart |

## Logging (`Options.hs:153-160`)

`--log-level` (global) or per-component: `--log-level-http-server`,
`--log-level-database`, `--log-level-consumer`,
`--log-level-garbage-collector`, `--log-level-configuration` (each default
`Info`). `tracerKupo` is hardcoded to `Info`. Severities: `Debug < Info <
Notice < Warning < Error`, plus `Off`.

## Subcommands

- `--version` / `-v`, and `version` — print version.
- `health-check --host --port` — Docker HEALTHCHECK-friendly probe.
- `copy --from DIR --into DIR [--match PATTERN...]` — bootstrap a narrower DB
  from a broader one.

# 2. HTTP API

Router: `src/Kupo/App/Http.hs:309-475`. Every path is also reachable under a
`/v1/` prefix. CORS: `Access-Control-Allow-Origin: *`, `OPTIONS` returns 200.

## Endpoints

| Method + Path | Notes |
|---|---|
| `GET /health` | content-negotiated (JSON or Prometheus); 200/202/503 by sync state |
| `GET /metrics` | same body as /health but always 200 |
| `GET /checkpoints` | streams all checkpoints (points), desc |
| `GET /checkpoints/{slot}` | `?strict` → exact slot; default → closest ancestor |
| `GET /matches` | all matches (see query params) |
| `GET /matches/{pattern}` | pattern as 1- or 2-fragment path param |
| `DELETE /matches/{pattern}` | prunes matches; `400 stillActivePattern` if it overlaps an active pattern; returns `{deleted:n}` |
| `GET /datums/{datum-hash}` | `{datum:<base16 CBOR>}` or `null` |
| `GET /scripts/{script-hash}` | script JSON or `null` |
| `GET /metadata/{slot}` | re-fetches block from node; `?transaction_id` filter; sets `X-Block-Header-Hash` |
| `GET /patterns` | list all active patterns |
| `GET /patterns/{pattern}` | patterns that *include* the given fragment (introspection) |
| `PUT /patterns` | bulk add: body `{patterns:[...], rollback_to:{...}, limit}` |
| `PUT /patterns/{pattern}` | add one pattern; body carries `rollback_to` |
| `DELETE /patterns/{pattern}` | remove pattern; returns `{deleted:n}` |

Unknown path → `404 notFound`; wrong method → `406 methodNotAllowed`.

## Query parameters on `GET /matches`

- `?spent` / `?unspent` — status filter.
- `?created_after=`, `?created_before=`, `?spent_after=`, `?spent_before=` —
  slot range; value may be a bare slot OR a full `slot.headerhash` point.
- `?order=most_recent_first` | `oldest_first`.
- `?resolve_hashes` — inline datum & script values.
- `?policy_id=`, `?asset_name=` (requires policy_id), `?transaction_id=`,
  `?output_index=` (requires transaction_id) — after-the-fact filters.
- `Accept: application/json;asset-quantity=string` — asset quantities as
  strings instead of integers.

## Match response shape (`resultToJson`, `Data/Pattern.hs:447-521`)

`transaction_index`, `transaction_id`, `output_index`, `address`, `value`,
`datum_hash`, (`datum`+`datum_type` when inline/resolved), `script_hash`,
(`script` when inline/resolved), `created_at:{slot_no,header_hash}`,
`spent_at:{slot_no,header_hash,transaction_id,input_index,redeemer}` or `null`.

## Caching / headers

- `ETag` = most-recent checkpoint header hash; `If-None-Match` → `304`.
- `X-Most-Recent-Checkpoint` header on data endpoints.
- 13 distinct JSON error responses with `{hint, details}` and specific
  400/404/406/500/503 codes (`Data/Http/Error.hs`).

# 3. Pattern language

Source: `src/Kupo/Data/Pattern.hs` (constructors `:115-138`, text codec
`:227-381`).

| Pattern | Text syntax |
|---|---|
| `MatchAny IncludingBootstrap` | `*` |
| `MatchAny OnlyShelley` | `*/*` |
| `MatchExact` | full address: base16, bech32 (`addr`/`addr_test`), or base58 (Byron) |
| `MatchPayment` | `<credential>/*` |
| `MatchDelegation` | `*/<credential>` or a `stake`/`stake_test` bech32 addr |
| `MatchPaymentAndDelegation` | `<credential>/<credential>` |
| `MatchTransactionId` | `*@<txid>` |
| `MatchOutputReference` | `<output_index>@<txid>` |
| `MatchPolicyId` | `<policyId>.*` |
| `MatchAssetId` | `<policyId>.<assetName>` |
| `MatchMetadataTag` | `{<tag>}` (decimal Word64) |

Credential syntax (`readerCredential`, `:307-335`): base16 (28-byte hash as-is,
32-byte key hashed to 28) or bech32 with HRPs `vk`/`addr_vk`/`stake_vk` (keys,
hashed) and `vkh`/`addr_vkh`/`stake_vkh`/`script` (hashes). Also exposes
`overlaps`/`includes`/`included` (`:143-221`), used by the `/patterns`
introspection endpoint and DELETE-overlap protection.

**Parity note:** `MatchMetadataTag` is **index-only** — usable as
`--match`/via `PUT`, but not queryable via `/matches` (see §4).

# 4. Metadata handling

**Kupo does not store transaction metadata anywhere.** No metadata table
(§5); `binary_data` holds Plutus datums, not tx metadata.

- Type: `Metadata = AlonzoTxAuxData ConwayEra` (`Data/Cardano/Metadata.hs:42`);
  all eras upgrade into it.
- **Tag matching (index time only):** during ingestion, `matchBlock` receives
  each tx's metadata and applies `MatchMetadataTag` via `hasMetadataTag`
  (`Pattern.hs:383-394`, `Metadata.hs:59-61`), reaching the matcher through
  `IsBlock`'s `mapMaybeOutputs :: (OutputReference -> Output -> Metadata ->
  Maybe result)`. The tag decides whether to index an output; **the metadata
  itself is discarded, never persisted.**
- **Serving `/metadata/{slot}`:** the handler finds the closest ancestor
  checkpoint, then **re-fetches the actual block from the chain producer at
  runtime** via a dedicated `FetchBlockClient` (`Data/FetchBlock.hs`,
  `App/Http.hs:786-827`), runs `userDefinedMetadata` per tx, and streams
  `{hash, raw (base16 CBOR), schema (typed JSON)}`, optionally filtered by
  `?transaction_id`.
- The `FetchBlockClient` is a *separate* connection (a dedicated pipelined
  ChainSync client, or a separate Ogmios socket). **Read-only replicas cannot
  serve `/metadata`** — no producer to fetch from
  (`UnableToFetchBlockFromReadOnlyReplica`).

Implication for the rewrite: metadata parity = (a) tag-based matching during
indexing from live block data, and (b) an on-demand block-refetch path from the
upstream source — *not* a metadata store.

# 5. Storage / persistence

Sources: `src/Kupo/App/Database/{Types,SQLite,Postgres}.hs`,
`src/Kupo/Data/Database.hs`, `db/`.

- **Backend selection**: compile-time CPP (`App/Database.hs:45-49`); `postgres`
  cabal flag default off. **Postgres module is a non-functional SQLite copy.**
- **`DatabaseLocation`** = `Dir | InMemory (Maybe FilePath) | Remote URI`. SQLite
  build rejects `Remote`; `Dir` → `<dir>/kupo.sqlite3`.
- **In-memory**: `InMemory Nothing` → shared-cache named memory DB (pools share
  it); `InMemory (Just fp)` → isolated (test-only). Lost on exit.
- **Tables** (final schema): `inputs` (UTxO; stored `ext_output_reference` PK,
  `address`, `value`, `datum_info`, `script_hash`, `created_at`, `spent_at`,
  `spent_by`, `spent_with`; generated virtual cols `output_reference`,
  `output_index`, `transaction_index`, `datum_hash`, `payment_credential`),
  `checkpoints`, `patterns`, `binary_data`, `scripts`, `policies` (FK→inputs
  `ON DELETE CASCADE`). **No metadata table.**
- **Indexes** (`installIndexes`, `SQLite.hs:1159-1205`): essential/permanent =
  `UNIQUE inputsByOutputReference` + PKs. Deferrable: `inputsByAddress`,
  `inputsByDatumHash`, `inputsByPaymentCredential`, `inputsByCreatedAt`,
  `inputsBySpentAt`, `policiesByPolicyId`. Temporary indexes are created/dropped
  around `pruneInputs`/`rollbackTo`. `--defer-db-indexes` skips them during
  sync; reaching tip raises `NodeTipHasBeenReached` and kupo restarts with
  indexes installed.
- **`--prune-utxo` (`RemoveSpentInputs`)** vs default (`MarkSpentInputs`),
  decided per block (`App.hs:558-577`): Mark → `UPDATE inputs SET
  spent_at/spent_by/spent_with`; Remove → `DELETE` only when the spend is deeper
  than `longestRollback` (129600 slots) from tip, else mark and defer to the
  gardener. Rollback un-sets `spent_at` for later spends.
- **Gardener/GC** (`App.hs:633-674`, interval `--gc-interval` default 3600s):
  (1) `pruneInputs` deletes spent inputs with `spent_at < MAX(checkpoint) -
  129600` (Remove mode only); (2) `pruneBinaryData` deletes orphan datums;
  (3) `PRAGMA optimize`. Both prune in 50000-row increments so the consumer can
  preempt.
- **Stability window = 129600 slots** (`3k/f` mainnet), hardcoded as
  `longestRollback` (`Options.hs:149`). Governs pruning safety, checkpoint
  spacing, and rollback bookkeeping.
- **`copy`**: byte-copies the source `.sqlite3`, wipes `inputs`/`policies`/
  `patterns`, inserts requested patterns, streams matching rows source→target,
  `VACUUM` + `optimize`.
- **Pooling/locking**: ReadOnly pool (`5×caps`), ReadWrite pool (`caps`), one
  exclusive long-lived writer coordinated by an in-process `DBLock`; WAL,
  `synchronous=NORMAL`, `foreign_keys=ON`.
- **10 migrations** embedded at compile time, keyed by `user_version`. Several
  are destructive resets requiring re-sync.

# 6. Sync / chain-following

Sources: `src/Kupo/App.hs`, `src/Kupo/App/ChainSync/{Node,Ogmios,Hydra}.hs`,
`Mailbox.hs`, `App/Configuration.hs`.

- **Architecture**: producer (network → `Mailbox`) + consumer (mailbox → DB), 4
  concurrent threads. Mailbox has a high-frequency queue for roll-forwards and a
  single-slot channel for rollbacks.
- **Three real producers + replica**:
  - **cardano-node**: Ouroboros ChainSync, pipelined with adaptive depth
    (100/5/1 in-flight by distance-to-tip).
  - **Ogmios**: WebSocket JSON-RPC; find-intersection then a fixed 100-deep
    `nextBlock` pipeline. TLS via `wss://`.
  - **Hydra**: WebSocket (`/?history=yes`); **no intersection protocol and no
    rollbacks** — resume simulated by skipping seen snapshots.
  - **ReadOnlyReplica**: no network; polls checkpoints every 5s.
- **Resume from checkpoints**: reads a sparse, exponentially-spaced checkpoint
  set from the DB and feeds it as the intersection candidate list. Checkpoints
  written on every roll-forward.
- **`--since`**: `origin`, `tip` (fetch node tip via `FetchTipClient`), or
  `SLOT.HEADERHASH`. Required on first start; conflicts with existing
  checkpoints are detected.
- **`--until`** (inclusive): consumer only rolls forward the satisfying prefix;
  the process never exits — it keeps serving queries.
- **Rollback** (`rollbackTo`): slot-based, strict — delete inputs `created_at >
  slot`, set `spent_at = NULL` where `spent_at > slot`, delete checkpoints `>
  slot`.
- **Spend recording**: keyed by `(spending txid, block slot)`; stores
  `spent_at`, `spent_by` (spending output ref), `spent_with` (redeemer).
- **Forced rollback via HTTP**: `PUT /patterns` with a `rollback_to` body;
  safe-zone enforced (`within_safe_zone` default vs
  `unsafe_allow_beyond_safe_zone`).
- **Two extra node connections**: a `FetchBlockClient` (metadata) and a
  `FetchTipClient` (`--since tip`), both rejected by Hydra/replica.

# 7. Operational

Sources: `src/Kupo/Data/Health.hs`, `src/Kupo/App/Health.hs`,
`App/Http/HealthCheck.hs`, `Control/MonadLog.hs`.

- **Metrics**: **no EKG store.** Prometheus text hand-rolled from an in-memory
  `Health` TVar per request. Exposed on `GET /metrics` (always 200) and
  `GET /health` with `Accept: text/plain`. `kupo_`-prefixed:
  `connection_status`, `most_recent_checkpoint`, `most_recent_node_tip`,
  `seconds_since_last_block`, `network_synchronization`,
  `configuration_indexes`. No latency/GC metrics.
- **`/health` JSON**: `connection_status`, `most_recent_checkpoint`,
  `most_recent_node_tip`, `seconds_since_last_block`, `network_synchronization`
  (5-dp), `configuration.indexes`, `version`. HTTP status: 200 (synced/near
  tip), 202 (connected, far behind), 503 (disconnected). Default `Accept: */*`
  returns Prometheus text, not JSON.
- **`health-check` command**: `GET /health`, exit 0 iff `connection_status ==
  "connected"`, else 1.
- **Logging/tracing**: 5 severities + `Off`; 6 tracer components. TTY → colored
  ANSI; non-TTY → newline-delimited structured JSON
  (`severity`/`timestamp`/`thread`/`message`/`version`).
- **Snapshot/restore**: none beyond `copy`; `--until` is the closest
  point-in-time mechanism.
- **TLS**: **no TLS on the API server** (plain Warp). `wss://`/TLS applies only
  to outbound Ogmios/Hydra connections.

# Parity cost tiers

**T1 — cheap / core (must-have, low complexity)**
- CLI: `--host`, `--port`, `--node-socket`+`--node-config`, `--since`,
  `--match`, `--workdir`, `--in-memory`, `--gc-interval`, `--log-level[-*]`,
  `--version`, `health-check`.
- HTTP: `/health`, `/metrics`, `/checkpoints`(+`/{slot}`+`?strict`),
  `/datums/{hash}`, `/scripts/{hash}`, `/patterns` GET/PUT/DELETE, CORS/OPTIONS,
  ETag/304, `X-Most-Recent-Checkpoint`, JSON error envelope.
- Pattern language: all constructors + text codec (bech32/base16/base58,
  credential hashing).
- Match query params: `?spent`/`?unspent`, `?order`, `?resolve_hashes`,
  `?policy_id`/`?asset_name`/`?transaction_id`/`?output_index`, slot-range
  params, `asset-quantity=string`.
- Storage: SQLite schema (6 tables, virtual columns), essential index,
  checkpoints, `spent_by`/`spent_with`/redeemer in results.
- Operational: structured JSON logging, health status codes, Prometheus text.

**T2 — moderate**
- Ogmios producer (WebSocket JSON-RPC + 100-deep pipelining + TLS).
- Adaptive pipelining for the node ChainSync client.
- `--defer-db-indexes` + auto-install-on-tip restart; the full non-essential
  index set and temporary-index management.
- Rollback correctness (slot-based delete/un-spend/checkpoint-delete) and resume
  via sparse exponential checkpoints.
- `--until` semantics (index-prefix, keep serving).
- `copy` command.
- `--read-only` replica mode.
- Forced rollback via `PUT /patterns` incl. safe-zone logic.
- Connection pooling + in-process write lock; DB migration engine.

**T3 — large lift**
- **PostgreSQL backend** — *does not exist in kupo either.* Real Postgres
  support would be net-new work that *exceeds* kupo; matching kupo literally
  means "SQLite only."
- **Metadata handling** — the on-demand block-refetch architecture: a dedicated
  second ChainSync/Ogmios connection (`FetchBlockClient`) fetching an arbitrary
  block by point, per-era `AuxData` extraction, typed metadata JSON, plus
  index-time `{tag}` matching. Most non-obvious subsystem; no DB backing.
- **Prune/GC + stability window** — the interplay of `--prune-utxo`
  eager-vs-deferred deletion, the 129600-slot immutability guarantee,
  incremental (50000-row) pruning that yields to the consumer, orphan
  binary-data collection, and rollback-driven un-spending. Correctness-critical.
- **Hydra producer** — layer-2 with no intersection/rollback protocol,
  snapshot-skipping resume (parity only if targeting Hydra at all).
- **Multi-era ledger coverage** — `IsBlock` across Byron→Conway for outputs,
  datums, scripts, redeemers, and metadata; tracks Cardano ledger releases.

# Caveats / could-not-confirm

- `--max-concurrency` is **not** a real flag (only in an error string). Do not
  implement it for parity.
- The `--host` help text's `wss://` note is misplaced (bind address vs
  Ogmios/Hydra outbound).
- Default port is **1442** (code), not 1337 (stale comment).
- `MatchMetadataTag` (`{tag}`) is index-only and **not** queryable via
  `/matches`.
