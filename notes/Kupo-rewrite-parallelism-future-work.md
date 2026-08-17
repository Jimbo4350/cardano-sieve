# Kupo Rewrite: Parallelism & Throughput — Future Work

Where the throughput headroom is, and what it costs to reach it. Companion to
[ADR-020](./Kupo-rewrite-ADR-020-indexing-architecture.md) and the
[threads-of-control note](./Kupo-rewrite-threads-of-control.md). Nothing here is
built yet — the current build is one core, inline decode, and this records the
path if measurement says we need it.

## The two levers (they do different things)

Throughput has a serial floor: the SQLite writer (one writer, blocks committed
in protocol order). Everything splits into two independent moves:

1. **Parallelize the pure work → gets you *to* the floor.**
   Decode (CBOR → block) and match (`selectedStored`/`spentInputs`) are pure CPU
   work. Fan them out across cores so the writer never waits on them. This
   raises throughput **up to the writer's rate, and no further.**

2. **Make the commit cheaper → *lowers* the floor.**
   The only way past the writer's rate is to make each commit cost less. This is
   a separate axis; upstream parallelism does nothing for it.

Be clear which lever you're pulling. If the commit is *already* the bottleneck,
lever 1 buys nothing — only lever 2 moves the needle.

## Lever 1: parallel decode + match

Prerequisites and constraints:

- **Needs `-threaded -N2+`.** On today's one-core build any parallelism is a
  no-op (sparks evaluate on demand with zero speedup). This is a build/deploy
  change first, code second.
- **The writer stays serial and in-order.** Shape is:
  `fan out (decode + match, pure) → reassemble in order → one serial writer`.
  Never parallelize the write — rollback and checkpoint correctness depend on
  protocol order and a single writer.
- **Use sparks (`rpar`), not a worker pool** — ADR-020 Decision 3 rung 2.
  Speculatively decode+match already-buffered blocks; the loop still consumes
  results in order and just finds them already evaluated. No second effectful
  thread, no queue.
- **Pipeline depth is the feedstock, sized to width — not a lever.**
  To keep `W` spare cores busy you need `depth ≥ W` + a little slack. Beyond
  that, more depth adds no parallelism (can't decode more at once than you have
  cores) and costs memory (≈ depth × max block size; overrunning the mux ingress
  queue kills the connection). Tie depth to `-N`, not to a big number.

## Lever 2: lower the write floor

The expensive part of a commit is the fsync, not the inserts.

- **Bigger batches (main lever).** Amortize one fsync over many rows —
  ADR-020 Decision 2 (byte-capped batching). **But it's a U-curve:** the tested
  sweet spot is ~50k rows per commit; past that, oversized transactions cost
  back in WAL size, memory, and commit latency. Sweet spot already found — don't
  assume bigger is better.
- **Durability trade.** `synchronous = NORMAL` + WAL defers fsyncs and would
  lower the floor further, but ADR-020 deliberately chose `FULL` for crash
  safety, betting batching makes the fsync affordable. A conscious knob, not an
  oversight — revisit only with a reason.
- **Cheaper per-row work.** Zero-copy writes (store the block's own CBOR slices
  instead of re-serializing — ADR-020 rung 1), fewer indexes.

## Measure first — the gate on all of it

ADR-020 is explicit: mitigation is earned by measurement, not assumed. Before
touching either lever, instrument the three stages (decode / match / write) and
find the bottleneck:

- If **decode+match dominates** → lever 1 (`-threaded -N`, sparks, depth to width).
- If **the commit dominates** → lever 1 is wasted; go straight to lever 2
  (batching / durability / zero-copy).
- Note the feedback loop: batching shrinks the write term, which *raises
  decode's relative share* — so lowering the floor can turn the commit-bound
  case into the decode-bound case. Re-measure after each change.

## One-line model

> `-threaded -N` supplies cores → **depth ≥ width** keeps them fed → **parallel
> decode+match** raises throughput up to the writer's rate → the writer's rate
> is lowered only by **batching / durability / cheaper writes** → and which
> lever helps is decided by **measurement**, not assumption.
