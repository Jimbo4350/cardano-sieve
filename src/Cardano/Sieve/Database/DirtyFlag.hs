{-# LANGUAGE OverloadedStrings #-}

-- | The dirty-flag protocol: crash detection for journal-less bulk writes.
--
-- @PRAGMA user_version@ is SQLite's application-owned header integer; SQLite
-- itself never interprets it. Sieve uses it as a dirty bit around the
-- 'Cardano.Sieve.Node.Insert.UnsafeBulk' window: 'markDirty' before the journal
-- goes off, 'markClean' on a clean exit. A crash inside the window dies before
-- reaching 'markClean', so a non-zero flag at open time ('isDirty') is proof the
-- file was abandoned mid-write — and with no journal that file may be corrupt in
-- ways nothing can detect, so it is refused ('DirtyDatabase').
module Cardano.Sieve.Database.DirtyFlag
  ( DirtyDatabase (..)
  , markDirty
  , markClean
  , isDirty
  )
where

import Control.Exception (Exception)
import Database.SQLite.Simple (Connection, Only (Only), execute_, query_)

-- | The file was left behind by an 'Cardano.Sieve.Node.Insert.UnsafeBulk'
-- session that never exited cleanly. With no journal there is no way to tell how
-- much of it is missing or mangled, so it is refused outright rather than
-- resumed from.
newtype DirtyDatabase = DirtyDatabase FilePath

instance Show DirtyDatabase where
  show (DirtyDatabase path) =
    unlines
      [ path <> ": left dirty by an interrupted bulk sync."
      , ""
      , "Catch-up runs with SQLite journaling off for speed, so a crash mid-sync"
      , "can corrupt the file in ways that cannot be detected, let alone repaired."
      , "Its contents cannot be trusted. Delete it and sync again."
      ]

instance Exception DirtyDatabase

-- | Flag the file dirty. Must run BEFORE the journal is disabled, so this write
-- itself still has crash protection.
markDirty :: Connection -> IO ()
markDirty conn = execute_ conn "PRAGMA user_version=1"

-- | Clear the flag, making the data it vouches for durable first: @synchronous@
-- is raised so the clearing write's commit fsyncs every dirty page of the file,
-- not just the header — the flag must not say \"trustworthy\" before the data is
-- on disk. Leaves @synchronous=FULL@; the caller sets its own level after.
markClean :: Connection -> IO ()
markClean conn = do
  execute_ conn "PRAGMA synchronous=FULL"
  execute_ conn "PRAGMA user_version=0"

-- | Was the file left behind by a bulk session that never exited cleanly?
isDirty :: Connection -> IO Bool
isDirty conn = do
  flags <- query_ conn "PRAGMA user_version" :: IO [Only Int]
  pure (case flags of Only flag : _ -> flag /= 0; [] -> False)
