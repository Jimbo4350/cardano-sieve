# cardano-sieve

A pattern-filtered chain index for Cardano: given a set of patterns, it
tracks every matching UTxO — when it was created, when it was spent, and by
what. It follows a local node over node-to-client ChainSync, writes matches
into SQLite, and serves them over an HTTP API shaped like
[kupo](https://cardanosolutions.github.io/kupo/)'s.

## Building

```bash
cabal build cardano-sieve -j4
```

## Usage

One executable, three modes, decided by which options you pass:

| mode | options | what it does |
|---|---|---|
| sync | `--socket-path` + `--testnet-magic` + `--database` | follow the chain and index matches |
| sync + serve | the above + `--serve PORT` | index and answer queries from the same process |
| serve only | `--database` + `--serve PORT` (no `--socket-path`) | serve an already-synced database, no node needed |

There is also `--build-indexes`, which installs the deferred query indexes on
`--database` and exits (see below).

### Indexing

```bash
cabal run cardano-sieve -- \
  --socket-path ~/node.socket \
  --testnet-magic 2 \
  --database sieve.sqlite \
  --select 'addr_test1...' \
  --select 'f66d78b4a3cb3d37afa0ec36461e51ecbde00f26c8f0a68f94b69880.*'
```

Options:

- `--select SELECTOR` — repeatable; an output is indexed when it matches *any*
  selector. Omit it entirely to index every output. On a restart against an
  existing database, omitting `--select` adopts whatever selectors that
  database was built with.
- `--since SLOT.HEADERHASH` — start point (default `origin`).
- `--until SLOT` — stop after this slot, inclusive (default: follow the chain
  forever).
- `--batch-size N` — commit to SQLite every N written rows (default 50000,
  tuned for bulk sync).
- `--with-redeemers` — also store the redeemer that authorised each spend
  (opt-in: redeemers are the heavy bytes on the spend path).

A sync running alone (no `--serve`) does catch-up in bulk mode — journaling
off, guarded by a dirty flag — and switches to durable commits on reaching the
tip. An unbounded sync builds the query indexes when it reaches the tip; a
bounded `--until` run deliberately does not, so after one, run:

```bash
cabal run cardano-sieve -- --database sieve.sqlite --build-indexes
```

### Serving queries

```bash
cabal run cardano-sieve -- --database sieve.sqlite --serve 1442
```

Serve-only mode never touches a node: `/health` reports the connection as
disconnected, and `/metadata/{slot-no}` (which is fetched from the node on
demand, never stored) answers 503.

Adding `--serve` to a sync invocation serves both from one process. Caveat:
while catching up, queries are only as fresh as the last committed batch —
at the tip a batch can sit unflushed for a while. Prefer this mode for a
bounded `--until` run (the final commit lands on exit), or serve a synced
database separately.

### Selectors

The same grammar is used by `--select` at ingest and by `/matches/{pattern}`
at query time, so the two cannot drift:

| syntax | matches |
|---|---|
| `*` | every output (Byron included) |
| an address (bech32/base58/base16) | that address |
| `payment/delegation` | address credential parts, hex or bech32; `*` in a slot leaves it free (e.g. `*/stake_test1...`) |
| `policyid.name` / `policyid.*` | one asset / a whole policy |
| `index@txid` / `*@txid` | one output / a whole transaction |
| `{n}` | metadata tag `n` |

### HTTP API

The endpoint surface and response shapes are kupo's (verified by diffing
against kupo v2.11):

| endpoint | |
|---|---|
| `GET /matches` / `GET /matches/{pattern}` | matching outputs; all thirteen of kupo's parameters (`?unspent`, `?spent`, `?order`, `?created_after`/`_before`, `?spent_after`/`_before`, `?policy_id`, `?asset_name`, `?transaction_id`, `?output_index`, `?resolve_hashes`) |
| `DELETE /matches/{pattern}` | prune everything a pattern matched; refused while a configured selector still covers it |
| `GET /datums/{hash}` | the datum behind a hash |
| `GET /scripts/{hash}` | the script behind a hash, with its language |
| `GET /checkpoints` | a sample of stored chain points, newest first |
| `GET /checkpoints/{slot-no}` | the point at-or-before a slot; `?strict` demands the exact slot |
| `GET /patterns` / `GET /patterns/{pattern}` | the configured selectors |
| `PUT`/`DELETE /patterns/...` | 501 — reconfiguring a live indexer is a non-feature; restart with different `--select`s |
| `GET /health` | JSON health, kupo-shaped |
| `GET /metrics` | the same facts in Prometheus exposition format |
| `GET /metadata/{slot-no}` | a block's transaction metadata, fetched from the node on demand; `?transaction_id` filters |

One deliberate divergence from kupo: where kupo streams every match in a
single unbounded response, sieve pages. A response carries at most 100 rows;
when more exist it sets an `X-Next-Cursor` header, and passing that value
back as `?after` resumes the walk:

```bash
curl -i 'http://localhost:1442/matches?unspent'
# ...
# X-Next-Cursor: 0000000000cbb1930001

curl 'http://localhost:1442/matches?unspent&after=0000000000cbb1930001'
```

The cursor is opaque — hand back exactly what the header carried. Every match
is reachable this way; a kupo client just has to learn to page.

## Notes

Design notes and decision records live in [`notes/`](./notes/), starting from
[ADR-020](./notes/Kupo-rewrite-ADR-020-indexing-architecture.md) and the
[parity checklist](./notes/Kupo-rewrite-parity-checklist.md). Benchmark
harnesses are in [`bench/`](./bench/) — `run-node.sh` starts a preview node,
`run-sync-bench.sh` and `run-query-bench.sh` measure sync and query
performance against kupo.
