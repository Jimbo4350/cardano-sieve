# Interning the output reference

Design note. Not yet implemented.

## Why

Sieve indexes preview `origin..2,000,000` in **39.0s wall / 38.3s CPU** producing a
**239 MB** database. Kupo does the same range in **29.5s / 24.2s** producing
**175 MB**, and both index an identical UTxO set (281,263 outputs ever, 65,723
unspent — the bench verifies this). Sieve burns 58% more CPU and writes 37% more
bytes.

Splitting sieve's own cost with a selector that matches nothing, so blocks are
still fetched, decoded and matched but nothing is written:

| phase | CPU |
|---|---|
| decode + match | 15.9s (35%) |
| write path | 29.5s (65%) |

**Sieve's write path alone exceeds kupo's entire run.** Decode is not the problem.

Where the 240 MB goes (`dbstat`, same run):

```
outputs                      66.1 MB  27.6%
sqlite_autoindex_policies_1  49.7 MB  20.7%
policies                     45.2 MB  18.9%
spends                       18.3 MB   7.7%
unspent                      17.8 MB   7.4%
sqlite_autoindex_outputs_1   15.0 MB   6.3%
sqlite_autoindex_spends_1    11.4 MB   4.8%
(the rest)                   16.5 MB   6.9%
```

`policies` plus its autoindex is **94.9 MB — 39.6% of the database**, for
759,035 rows. Inside those rows:

```
output_reference   30.4 MB    ← 84% of the row bytes
asset_name          4.3 MB
policy_num          1.5 MB
```

Every column appears **twice**, because the primary key is
`(output_reference, policy_num, asset_name)` — all three again in the autoindex.
So roughly **61 MB of the 94.9 MB is one 40-byte output reference, repeated**.

## What changes

Give `outputs` an integer surrogate and have the two big join-only tables carry
that instead of the blob.

```sql
outputs  ( output_num       INTEGER PRIMARY KEY        -- rowid alias
         , output_reference BLOB NOT NULL UNIQUE
         , transaction_id   BLOB GENERATED ALWAYS AS (substr(output_reference,1,32))
         , … unchanged … )

policies ( output_num   INTEGER NOT NULL              -- was BLOB(40)
         , policy_num   INTEGER NOT NULL
         , asset_name   BLOB    NOT NULL
         , created_slot INTEGER NOT NULL
         , PRIMARY KEY (output_num, policy_num, asset_name) )

spends   ( output_num INTEGER NOT NULL PRIMARY KEY    -- was BLOB(40)
         , … unchanged … )

unspent  ( output_reference BLOB NOT NULL PRIMARY KEY -- KEPT, see below
         , output_num       INTEGER NOT NULL UNIQUE   -- added, for joins
         , … unchanged … )
```

**`unspent` keeps the reference.** It is the query-hot table: results return the
output reference, and `?transaction_id` reads the generated column derived from
it. Joining out to `outputs` for every result row would trade a large write-side
saving for a per-row read-side cost. At 65,723 rows the blob costs ~2.6 MB, which
is not worth reclaiming.

### The surrogate is free on the insert path

`insertOutput` writes the output immediately before its policy rows, so
`last_insert_rowid()` hands over the surrogate with no lookup at all. This is the
opposite of `policyNumOf`, which needs a lookup because policies recur across
outputs.

### And free on the spend path

`recordSpend` already probes `outputs` by reference:

```sql
INSERT OR IGNORE INTO spends (output_reference, …)
SELECT ?,?,?,?,? WHERE EXISTS (SELECT 1 FROM outputs WHERE output_reference = ?)
```

That `EXISTS` becomes a projection of the column we now want — same index probe,
same one statement:

```sql
INSERT OR IGNORE INTO spends (output_num, …)
SELECT output_num, ?,?,?,? FROM outputs WHERE output_reference = ?
```

The `DELETE FROM unspent` that follows needs the surrogate too, so this becomes a
`SELECT` then two statements rather than two statements. One extra round of
binding per spend, no extra index probe.

## Expected saving

| | rows | bytes/row saved | ×2 for index | total |
|---|---|---|---|---|
| `policies` | 759,035 | ~37 | yes | ~56 MB |
| `spends` | 215,540 | ~37 | yes | ~16 MB |
| | | | | **~72 MB** |

240 MB → **~168 MB**, against kupo's 175 MB. From 37% larger to slightly smaller.

Write CPU should fall with it, though by how much is a guess until measured —
fewer bytes is fewer pages is less journal traffic, but the statement count is
unchanged and that is the other half of the cost.

## Asset-name interning: rejected, and why

The obvious companion — a dictionary for asset names, as Jordan proposed
alongside this — **should not be done**.

It would save ~7 MB (4.3 MB in the table, doubled for the index). But unlike the
output surrogate it is **not free**: asset names recur across outputs, so every
one of the 759,035 policy rows needs a dictionary lookup, exactly like
`policyNumOf`. That path is already the single largest cost in the write phase at
**1,518,070 statements**, ~45% of all SQL executed. Adding 759,035 more lookups to
reclaim 7 MB trades the expensive resource for the cheap one.

There are only 2,776 distinct `(policy, asset)` pairs against 877 policies, so the
dictionary itself would be tiny — the cost is entirely in the per-row lookups.

Revisit only if `policyNumOf` is made cheap enough that another lookup per row
stops mattering.

## Keep `policy_num` and `asset_name` as separate columns

Not a single surrogate for the `(policy, asset)` pair, tempting though 2,776
distinct pairs makes it. A pair surrogate cannot be decomposed back into a
policy, so `SelectPolicyId` degrades from one index seek to `asset_id IN (…)` — N
disjoint ranges whose concatenation is not slot-ordered, forcing a
materialise-and-sort. Measured at 2.3s against sub-ms for the hottest policy. The
existing schema note in `Schema.hs` says the same; this does not change it.

## Work

1. **Schema** — `outputs.output_num`, swap the column in `policies` and `spends`,
   add `unspent.output_num`. Indexes on `policies` keep `policy_num` leading, so
   `policiesByPolicyId` and `policiesByAssetId` change only their first column.
2. **`insertOutput`** — take the rowid from the output insert, use it for the
   policy rows. Both `outputs` and `unspent` inserts need it.
3. **`recordSpend`** — resolve reference to surrogate once, use for both the
   `spends` insert and the `unspent` delete.
4. **`rollbackAbove`** — the unspent-restore subquery becomes
   `output_num IN (SELECT output_num FROM spends WHERE spent_slot > ?)`.
5. **`Http.hs`** — `policiesExists` joins on `output_num`; the `spends` LEFT JOIN
   joins on `output_num`; `?transaction_id`/`?output_index` still filter `unspent`
   by reference, unchanged.
6. **No migration.** Schema changes are wipe-and-resync during development, as the
   header of `Schema.hs` states.

## How to verify

- **Identical data first, before timing anything.** Sync `origin..2,000,000`
  before and after; assert equal counts for `outputs`, `unspent`, `policies`,
  `spends`, and that the set of `(output_reference, policy_id, asset_name)` triples
  is unchanged — joining back through both dictionaries. Byte-identical, or it
  does not ship.
- **`bench/run-query-compare.sh`** for the read path: `?policy_id` and
  `?asset_name` must stay index-served. `EXPLAIN QUERY PLAN` must not show
  `USE TEMP B-TREE FOR ORDER BY`, which is the signal that the composite index
  stopped covering the sort.
- **`bench/run-flush-check.sh`** with `BASELINE_REF` at the pre-change commit for
  write throughput, and `dbstat` for the size claim.
- **The rollback tests already in `test/Main.hs`** cover the invariant this touches
  most — an output is in `unspent` exactly when it has no `spends` row.

## What this does not fix

`policyNumOf`'s 1,518,070 statements, which is the largest single item in the
write phase. That is a separate question, and the measured options are recorded in
its haddock: an in-memory `Map` (−13.9% of total runtime), the same two statements
prepared once (−9.8%), or a per-block `withStatement` scope, which is the only
variant not yet measured and the only one that keeps policy state out of
`DbHandle`.
