{-# LANGUAGE OverloadedStrings #-}

-- | Persistence layer: dump block headers into a SQLite database.
--
-- This is the Phase-1 storage milestone from ADR-020 ("just drop block headers
-- into the database for now"). It uses 'sqlite-simple' — the same library kupo
-- uses — so throughput/memory comparisons stay apples to apples.
--
-- Writes are /batched/: inserts accumulate inside a single open transaction and
-- only COMMIT every N rows, amortising SQLite's per-commit @fsync@ over the
-- batch. The open transaction is SQLite's own buffer, so this needs no
-- application-level queue (ADR-020 Decision 1). The final partial batch is
-- committed by 'closeDatabase'; run it via 'Control.Exception.finally' so it
-- also fires when the caller is torn down by an async exception.
module Cardano.Sieve.Node.Insert
  ( DbHandle
  , BlockHeaderRow (..)
  , openDatabase
  , closeDatabase
  , writeHeader
  , rollbackAbove
  )
where

import Control.Exception (onException)
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple
  ( Connection
  , Only (Only)
  , ToRow (toRow)
  , close
  , execute
  , execute_
  , open
  , query_
  )

-- | One row of the @block_header@ table.
data BlockHeaderRow = BlockHeaderRow
  { rowBlockNo :: Int64
  , rowSlotNo :: Int64
  , rowHash :: Text
  }

instance ToRow BlockHeaderRow where
  toRow (BlockHeaderRow b s h) = toRow (b, s, h)

-- | A handle to the block-header SQLite database; header writes are batched.
data DbHandle = DbHandle
  { dbConn :: Connection
  , dbBatchSize :: Int
  -- ^ COMMIT once this many rows have accumulated in the open transaction.
  , dbUncommittedRows :: IORef Int
  -- ^ Rows written into the open transaction but not yet committed; climbs to
  -- 'dbBatchSize', then a COMMIT resets it to 0 (also the open-transaction flag:
  -- 0 = no transaction open).
  }

-- | Open the database, prepare it (pragmas + schema) and return a batched
-- 'DbHandle'. Pair every 'openDatabase' with 'closeDatabase' — via
-- 'Control.Exception.finally' — so the final partial batch is always committed.
openDatabase :: FilePath -> Int -> IO DbHandle
openDatabase path batchSize = do
  conn <- open path
  -- If preparing the freshly-opened connection throws, close it rather than
  -- leak the handle (the caller only gets to 'closeDatabase' a 'DbHandle' we return).
  prepare conn `onException` close conn
  pending <- newIORef 0
  pure (DbHandle conn (max 1 batchSize) pending)

-- | Commit the final partial batch (if any) and close the connection.
closeDatabase :: DbHandle -> IO ()
closeDatabase db = do
  flush db
  close (dbConn db)

-- | Ready a freshly-opened connection for batched writes: set the session
-- PRAGMAs, then create the @block_header@ table if it is not already there.
--
-- @journal_mode=WAL@ lets the writer commit without blocking readers and keeps
-- each commit cheap; @synchronous=NORMAL@ replaces the per-commit @fsync@ with
-- one at each WAL checkpoint. The database is never left corrupt, and a process
-- crash loses nothing (a commit still writes the WAL frames to the OS, it just
-- skips the fsync); only a power loss or OS crash can roll back transactions
-- committed since the last checkpoint. Together with batching, that is where
-- the write throughput comes from. @CREATE TABLE IF NOT EXISTS@ makes reopening
-- an existing database a no-op.
--
-- The PRAGMAs go through 'query_' rather than 'execute_' because @PRAGMA
-- journal_mode@ returns a row (the mode it settled on) and 'execute_' rejects
-- statements that produce output.
prepare :: Connection -> IO ()
prepare conn = do
  mapM_
    (\q -> () <$ (query_ conn q :: IO [Only Text]))
    [ "PRAGMA journal_mode=WAL"
    , "PRAGMA synchronous=NORMAL"
    ]
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS block_header (block_no INTEGER PRIMARY KEY, slot_no INTEGER NOT NULL, hash TEXT NOT NULL)"

-- | Insert one header. Opens a transaction lazily on the first row of a batch
-- and commits once 'dbBatchSize' rows have accumulated. @INSERT OR REPLACE@
-- keeps it idempotent across restarts and re-syncs.
writeHeader :: DbHandle -> BlockHeaderRow -> IO ()
writeHeader (DbHandle conn batchSize pending) row = do
  n <- readIORef pending
  when (n == 0) $ execute_ conn "BEGIN TRANSACTION"
  execute
    conn
    "INSERT OR REPLACE INTO block_header (block_no, slot_no, hash) VALUES (?, ?, ?)"
    row
  let n' = n + 1
  if n' >= batchSize
    then execute_ conn "COMMIT" >> writeIORef pending 0
    else writeIORef pending n'

-- | Commit the currently open (partial) batch, if any.
flush :: DbHandle -> IO ()
flush (DbHandle conn _ pending) = do
  n <- readIORef pending
  when (n > 0) $ execute_ conn "COMMIT" >> writeIORef pending 0

-- | On a chain rollback, drop persisted headers strictly newer than the
-- rollback point. 'Nothing' means roll back to genesis (delete everything).
-- Commits any open batch first so the delete sees a consistent table.
rollbackAbove :: DbHandle -> Maybe Int64 -> IO ()
rollbackAbove db@(DbHandle conn _ _) mSlot = do
  flush db
  case mSlot of
    Nothing -> execute_ conn "DELETE FROM block_header"
    Just slot -> execute conn "DELETE FROM block_header WHERE slot_no > ?" (Only slot)
