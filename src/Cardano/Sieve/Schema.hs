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
    \, transaction_id   BLOB    NOT NULL \
    \, address          BLOB    NOT NULL \
    \, value            BLOB    NOT NULL \
    \, datum_hash       BLOB \
    \, script_hash      BLOB \
    \, created_slot     INTEGER NOT NULL \
    \)"
  , -- The live UTxO set: INSERT on create, DELETE by primary key on spend.
    -- Thin-but-covering — carries the columns queries filter and return, and
    -- holds all the query indexes, joining out only for heavy preimages.
    "CREATE TABLE IF NOT EXISTS unspent \
    \( output_reference      BLOB    NOT NULL PRIMARY KEY \
    \, transaction_id        BLOB    NOT NULL \
    \, address               BLOB    NOT NULL \
    \, payment_credential    BLOB \
    \, delegation_credential BLOB \
    \, value                 BLOB    NOT NULL \
    \, datum_hash            BLOB \
    \, script_hash           BLOB \
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
    -- coupled to the Phase-8 surface syntax; until then it is a canonical
    -- serialisation of a 'Cardano.Sieve.Selector.Selector'.
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
  , -- Policy index backing SelectPolicyId / SelectAssetId, over the /full/
    -- history (append-only, never deleted) so spent-inclusive policy queries
    -- stay indexed. Unspent-by-policy joins these hits to @unspent@ by primary
    -- key.
    "CREATE TABLE IF NOT EXISTS policies \
    \( output_reference BLOB NOT NULL \
    \, policy_id        BLOB NOT NULL \
    \, PRIMARY KEY (output_reference, policy_id) \
    \, FOREIGN KEY (output_reference) REFERENCES outputs(output_reference) \
    \)"
  ]

-- All secondary indexes live on the small @unspent@ table (plus @policies@ and
-- @spends@); @outputs@ stays primary-key-only, so appending to history is cheap
-- and spent-inclusive/historical queries are best-effort.
indexes :: [Query]
indexes =
  [ "CREATE INDEX IF NOT EXISTS unspentByAddress              ON unspent(address)"
  , "CREATE INDEX IF NOT EXISTS unspentByPaymentCredential    ON unspent(payment_credential)"
  , "CREATE INDEX IF NOT EXISTS unspentByDelegationCredential ON unspent(delegation_credential)"
  , "CREATE INDEX IF NOT EXISTS unspentByTransactionId        ON unspent(transaction_id)"
  , "CREATE INDEX IF NOT EXISTS unspentByCreatedSlot          ON unspent(created_slot)"
  , "CREATE INDEX IF NOT EXISTS policiesByPolicyId            ON policies(policy_id)"
  , "CREATE INDEX IF NOT EXISTS spendsBySlot                  ON spends(spent_slot)"
  ]
