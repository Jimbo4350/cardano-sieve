# cardano-sieve

A pattern-filtered chain index for Cardano: given a set of patterns, it
tracks every matching UTxO — when it was created, when it was spent, and by
what.

## Building

```bash
cabal build cardano-sieve -j4
```
