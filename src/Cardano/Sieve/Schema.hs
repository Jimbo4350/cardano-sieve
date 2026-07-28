{-# LANGUAGE OverloadedStrings #-}

-- | The SQLite schema — the authoritative definition of how matched data is
-- stored on disk. See @notes/Kupo-rewrite-pattern-and-storage-design.md@ for the
-- design and its rationale.
--
-- Three core tables split kupo's single match table by write pattern:
--
--   * @outputs@ — append-only history of every matched output; never mutated.
--   * @unspent@ — the live UTxO set; INSERT on create, DELETE by primary key on
--     spend. Thin-but-covering, and where the query indexes live, so index
--     maintenance scales with the (small) live set rather than all of history.
--   * @spends@ — append-only spend provenance, keyed by the consumed output.
--
-- supported by @blocks@ (durable slot → header-hash, for @created_at@ /
-- @spent_at@ in results), the deduplicated @binary_data@ and @scripts@ preimage
-- stores, @policies@ (the policy/asset index, over full history), and the
-- @patterns@ / @checkpoints@ bookkeeping tables.
--
-- There is no migration engine yet: tables are created with @CREATE TABLE IF NOT
-- EXISTS@ and schema changes are handled by wipe-and-resync during development
-- (as kupo itself does between versions). A @user_version@-keyed migration
-- system can be added later if forward-compatible upgrades are needed.
--
-- Secondary indexes are /deferred/ ('installDeferredIndexes') until the initial
-- catch-up sync reaches the tip, so the write-heavy catch-up pays only
-- primary-key maintenance.
module Cardano.Sieve.Schema
  ( createSchema
  , installDeferredIndexes
  )
where

import Database.SQLite.Simple (Connection, Query, execute_)

-- | Create every table if it does not already exist. Only the primary keys are
-- indexed here; the secondary indexes that back queries are deferred (see
-- 'installDeferredIndexes'). Foreign-key /enforcement/ is the caller's job:
-- set @PRAGMA foreign_keys = ON@ on the connection.
createSchema :: Connection -> IO ()
createSchema conn = mapM_ (execute_ conn) tables

-- | Install the secondary indexes that back the query API. Run once, after the
-- initial catch-up sync has reached the chain tip.
installDeferredIndexes :: Connection -> IO ()
installDeferredIndexes conn = mapM_ (execute_ conn) indexes

-- Table order matters: @policies@ has a foreign key into @outputs@, so
-- @outputs@ must be created first.
tables :: [Query]
tables =
  [ -- One row per block that produced a matched output or a spend. Never
    -- pruned, so any historical slot's header hash stays recoverable — the
    -- durable slot → header-hash source that (sparse, pruned) @checkpoints@
    -- cannot be. Populated with INSERT OR IGNORE alongside output/spend writes.
    "CREATE TABLE IF NOT EXISTS blocks \
    \( slot_no     INTEGER NOT NULL PRIMARY KEY \
    \, header_hash BLOB    NOT NULL \
    \)"
  , -- Append-only full history: every matched output ever created. Never
    -- mutated. The header hash for created_slot is obtained by joining @blocks@.
    "CREATE TABLE IF NOT EXISTS outputs \
    \( output_reference BLOB    NOT NULL PRIMARY KEY \
    \, transaction_id   BLOB    GENERATED ALWAYS AS (substr(output_reference, 1, 32)) VIRTUAL \
    \, address          BLOB    NOT NULL \
    \, value            BLOB    NOT NULL \
    \, datum_hash       BLOB \
    \, reference_script_hash BLOB \
    \, created_slot     INTEGER NOT NULL \
    \)"
  , -- The live UTxO set: INSERT on create, DELETE by primary key on spend.
    -- Thin-but-covering — carries the columns queries filter and return, and
    -- holds all the query indexes, joining out only for heavy preimages.
    "CREATE TABLE IF NOT EXISTS unspent \
    \( output_reference      BLOB    NOT NULL PRIMARY KEY \
    \, transaction_id        BLOB    GENERATED ALWAYS AS (substr(output_reference, 1, 32)) VIRTUAL \
    \, address               BLOB    NOT NULL \
    \, payment_credential    BLOB \
    \, delegation_credential BLOB \
    \, value                 BLOB    NOT NULL \
    \, datum_hash            BLOB \
    \, reference_script_hash BLOB \
    \, created_slot          INTEGER NOT NULL \
    \)"
  , -- Append-only spend provenance, keyed by the consumed output reference. The
    -- header hash for spent_slot is obtained by joining @blocks@.
    "CREATE TABLE IF NOT EXISTS spends \
    \( output_reference        BLOB    NOT NULL PRIMARY KEY \
    \, spending_transaction_id BLOB    NOT NULL \
    \, spending_input_index    INTEGER NOT NULL \
    \, spent_slot              INTEGER NOT NULL \
    \, redeemer                BLOB \
    \)"
  , -- The set of active selectors, so they survive restarts. The text form is
    -- coupled to the (not-yet-defined) surface syntax; until then it is a
    -- canonical serialisation of a 'Cardano.Sieve.Selector.Selector'.
    "CREATE TABLE IF NOT EXISTS patterns \
    \( selector TEXT NOT NULL PRIMARY KEY \
    \)"
  , -- Sparse, pruned resume points for re-finding the chain intersection on
    -- restart. Distinct from @blocks@: this is a thinned subset of /processed/
    -- points (including empty blocks), not every block with matched activity.
    "CREATE TABLE IF NOT EXISTS checkpoints \
    \( slot_no     INTEGER NOT NULL PRIMARY KEY \
    \, header_hash BLOB    NOT NULL \
    \)"
  , -- Deduplicated datum preimages, referenced by hash from outputs/unspent.
    "CREATE TABLE IF NOT EXISTS binary_data \
    \( datum_hash BLOB NOT NULL PRIMARY KEY \
    \, datum      BLOB NOT NULL \
    \)"
  , -- Deduplicated script preimages, referenced by hash from outputs/unspent.
    "CREATE TABLE IF NOT EXISTS scripts \
    \( script_hash BLOB NOT NULL PRIMARY KEY \
    \, script      BLOB NOT NULL \
    \)"
  , -- Policy/asset index backing SelectPolicyId / SelectAssetId, over the /full/
    -- history (append-only, never deleted) so spent-inclusive policy/asset
    -- queries stay indexed. Unspent-by-policy/asset joins these hits to @unspent@
    -- by primary key. One row per (output, policy_id, asset_name) — an output can
    -- hold several assets under one policy, so asset_name is part of the key.
    -- @created_slot@ is denormalised from the output so the newest-first sort is
    -- covered by the composite indexes below, without joining @unspent@ to sort.
    --
    -- KNOWN HOT SPOT / redesign escalation (if we need to change this table):
    -- even with the composite indexes this table is full-history, so a query for
    -- a hot policy still walks spent entries. If the kupo comparison shows we
    -- lose here, give the UNSPENT policy/asset path its own live-set table,
    -- mirroring outputs-vs-unspent:
    --   unspent_policies(output_reference, policy_id, asset_name, created_slot),
    --   insert-on-create / delete-on-spend like @unspent@, indexed
    --   @(policy_id, created_slot)@ and @(policy_id, asset_name, created_slot)@ —
    --   no spent entries to skip. Consistent with the "index the small live set"
    --   philosophy, but it adds ingest write work (a tier-1 fast-sync cost), so
    --   build it only once measured against kupo. See [[sieve-query-index-tuning]].
    "CREATE TABLE IF NOT EXISTS policies \
    \( output_reference BLOB    NOT NULL \
    \, policy_id        BLOB    NOT NULL \
    \, asset_name       BLOB    NOT NULL \
    \, created_slot     INTEGER NOT NULL \
    \, PRIMARY KEY (output_reference, policy_id, asset_name) \
    \, FOREIGN KEY (output_reference) REFERENCES outputs(output_reference) \
    \)"
  ]

-- All secondary indexes live on the small @unspent@ table (plus @policies@ and
-- @spends@); @outputs@ stays primary-key-only, so appending to history is cheap
-- and spent-inclusive/historical queries are best-effort.
--
-- The point-dimension indexes are COMPOSITE @(filter, created_slot)@ so a single
-- index serves both the @WHERE@ equality and the newest-first @ORDER BY
-- created_slot@: SQLite reads the rows already in order (no separate sort pass)
-- and @LIMIT@ stops early. Single-column versions were net-negative for hot keys
-- — 126 ms vs 0.2 ms composite (query bench, 2026-07-28).
--
-- Two deliberate choices:
--   * NOT covering. The composite ends at @created_slot@, not the returned
--     @value@/@datum_hash@ — SQLite locates rows via the index, then fetches
--     their columns by primary key. Adding @value@ (a large blob) to the index
--     would bloat it for no measurable gain.
--   * @DESC@ needs no special index. SQLite reads an ascending index backwards
--     to satisfy @ORDER BY created_slot DESC@, so the index is defined ascending.
indexes :: [Query]
indexes =
  [ "CREATE INDEX IF NOT EXISTS unspentByAddress              ON unspent(address, created_slot)"
  , "CREATE INDEX IF NOT EXISTS unspentByPaymentCredential    ON unspent(payment_credential, created_slot)"
  , "CREATE INDEX IF NOT EXISTS unspentByDelegationCredential ON unspent(delegation_credential, created_slot)"
  , "CREATE INDEX IF NOT EXISTS unspentByPaymentAndDelegation ON unspent(payment_credential, delegation_credential, created_slot)"
  , "CREATE INDEX IF NOT EXISTS unspentByTransactionId        ON unspent(transaction_id, created_slot)"
  , "CREATE INDEX IF NOT EXISTS unspentByCreatedSlot          ON unspent(created_slot)"
  , "CREATE INDEX IF NOT EXISTS policiesByPolicyId            ON policies(policy_id, created_slot)"
  , "CREATE INDEX IF NOT EXISTS policiesByAssetId             ON policies(policy_id, asset_name, created_slot)"
  , "CREATE INDEX IF NOT EXISTS spendsBySlot                  ON spends(spent_slot)"
  ]
