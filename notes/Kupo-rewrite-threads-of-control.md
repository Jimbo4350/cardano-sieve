# Kupo Rewrite: Runtime Threads of Control

Companion to [ADR-020](./Kupo-rewrite-ADR-020-indexing-architecture.md): a quick
picture of what actually runs when `cardano-sieve` syncs.

## The key fact

The executable is built **without `-threaded`**, so the whole process runs on
**one OS thread / one core**. Several lightweight (green) threads take turns on
it — real concurrency, but no parallelism.

Two kinds of waiting behave differently on this runtime:

- **Waiting on the network** — the thread steps aside and others run.
- **Inside an SQLite call** (a C call) — it holds the one OS thread, so
  *everything else freezes* until it returns.

## The threads

- **main** — calls `connectToLocalNode` and just waits there the whole run.
- **mux ingress** — reads bytes off the socket, sorts them by protocol.
- **mux egress** — writes our requests out to the socket.
- **ChainSync** — the indexing loop: decode → match → write to SQLite.
  **All the real work is here** (not on main).

## Picture

```
cardano-node  ── serves raw-CBOR blocks over the socket
     │
     ▼  TCP socket  →  kernel buffer (the OS fills this even while we're frozen)
     │
┌────────────────────────────────────────────────────────────┐
│ cardano-sieve — ONE core, threads take turns:               │
│                                                              │
│   main        asleep in connectToLocalNode                   │
│   mux egress  sends "give me the next block" requests        │
│   mux ingress socket → sorts bytes → ChainSync's queue       │
│                                                              │
│   ChainSync   ◀── the loop:                                  │
│       keep ~50 requests in flight                            │
│       pop the next reply (already buffered, no wait)         │
│       decode CBOR → block                                    │
│       match block against selectors                          │
│       insert into SQLite   ← C call: freezes everything else │
└──────────────────────────────────────────────────────────────┘
```

## How a block becomes rows

1. Find the start point, then fire ~50 "next block" requests without waiting
   (pipelining).
2. The node streams replies; they pile up in the kernel/mux buffers.
3. The loop pops the next reply (a local queue pop, not a network round-trip),
   decodes it, matches it, and inserts it — committing to SQLite in batches.
4. While that SQLite commit runs, the whole process is frozen — but the node was
   already asked for ~50 blocks and the kernel keeps buffering them, so the next
   block is usually already waiting when the loop resumes.
5. Rollbacks arrive on the same thread in order, so nothing can get out of order.

The 50-deep pipeline is what hides network latency behind the serialized
decode+write. There is no application-level queue: if the loop slows down it
stops requesting, the node stops sending, and memory stays bounded.

## If we ever add `-threaded -N2+`

The mux could keep pulling blocks on a second core *while* SQLite writes on the
first — real fetch/write overlap — and ADR-020's spark-based decode speedup
would finally do something (it needs a second core). The catch: we'd lose the
"one thread, so it's automatically in order and bounded" property, so we'd
likely need a small bounded queue between the mux and the writer. The current
one-core build is the deliberate starting point — measure first, add threads
only if the numbers say so.
