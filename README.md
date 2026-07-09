# cardano-sieve

A pattern-filtered chain index for Cardano: given a set of patterns, it
tracks every matching UTxO — when it was created, when it was spent, and by
what.

Reimplementation of [kupo](https://github.com/IntersectMBO/kupo) on a
single-threaded pipelined indexing architecture; see
[ADR-020](https://github.com/input-output-hk/cardano-node-wiki/wiki/ADR-020-Kupo-rewrite-indexing-architecture)
for the design.

## Building

```bash
cabal build cardano-sieve -j4
```
