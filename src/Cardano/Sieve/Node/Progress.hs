-- | Sync progress reporting: the rolling counters behind the periodic
-- heartbeat line, and the timestamped log line everything is printed with.
module Cardano.Sieve.Node.Progress
  ( Progress
  , newProgress
  , tick
  , summarise
  , heartbeatSeconds
  , logLine
  , commas
  , duration
  )
where

import Cardano.Slotting.Slot (SlotNo, unSlotNo)

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)

-- | Rolling counters behind the periodic sync heartbeat.
--
-- A bulk sync processes millions of blocks, so a line per block is unreadable
-- and costs real throughput in the hot loop (it is why the benchmark used to
-- discard sieve's output wholesale). Instead every roll-forward folds its work
-- into these counters — cheap, no I/O — and a line is emitted only once
-- 'heartbeatSeconds' have passed.
data Progress = Progress
  { pgStartedAt :: !Double
  -- ^ Monotonic seconds when the sync began; the basis for @elapsed@.
  , pgReportedAt :: !Double
  -- ^ Monotonic seconds when the last line was emitted. Also the left edge of
  -- the window the reported block rate is computed over.
  , pgBlocks :: !Int
  -- ^ Blocks rolled forward since the start.
  , pgOutputs :: !Int
  -- ^ Matched outputs written since the start.
  , pgSpends :: !Int
  -- ^ Spends recorded since the start.
  , pgBlocksAtReport :: !Int
  -- ^ 'pgBlocks' as of the last emitted line, so the rate is the /recent/ rate
  -- rather than a start-to-now average that hides a slowdown.
  , pgSlotAtReport :: !Word64
  -- ^ Slot reached as of the last emitted line. Drives the ETA, which needs a
  -- /slot/ rate rather than the block rate: the distance left to cover is
  -- measured in slots, and on a chain with empty slots the two differ.
  }

-- | Emit at most one progress line per this many seconds.
heartbeatSeconds :: Double
heartbeatSeconds = 5

newProgress :: IO (IORef Progress)
newProgress = do
  now <- getMonotonicTime
  newIORef (Progress now now 0 0 0 0 0)

-- | Fold one block's work into the counters, emitting a progress line if the
-- heartbeat interval has elapsed. One clock read per block on the common path.
--
-- @target@ is the slot the sync is heading for, when known — @--until@ for a
-- bounded run, the server's tip for a following one — and drives the percentage.
tick :: IORef Progress -> SlotNo -> Maybe SlotNo -> Int -> Int -> IO ()
tick ref slotNo target outputs spends = do
  now <- getMonotonicTime
  pg <- readIORef ref
  let folded =
        pg
          { pgBlocks = pgBlocks pg + 1
          , pgOutputs = pgOutputs pg + outputs
          , pgSpends = pgSpends pg + spends
          }
  if now - pgReportedAt folded < heartbeatSeconds
    then writeIORef ref folded
    else do
      writeIORef
        ref
        folded
          { pgReportedAt = now
          , pgBlocksAtReport = pgBlocks folded
          , pgSlotAtReport = unSlotNo slotNo
          }
      logLine (progressLine folded now slotNo target)

-- | The heartbeat line, e.g.
--
-- > 14:22:07  syncing   32.1%  slot 1,284,213/3,999,989  1,843 blk/s  eta 24m35s  blocks 61,204  outputs 418,337  spends 205,118  elapsed 35s
--
-- and once there is no target left to head for:
--
-- > 14:48:19  at tip    slot 3,999,989  12 blk/s  blocks 1,204,551  outputs 8,418,337  spends 7,205,118  elapsed 26m12s
progressLine :: Progress -> Double -> SlotNo -> Maybe SlotNo -> String
progressLine pg now slotNo target =
  case target of
    Just t | unSlotNo t > unSlotNo slotNo -> heading t
    -- No target, or we have caught up with it. A percentage and an ETA are
    -- meaningless here, and printing "100.0%" every 5s while following the tip
    -- reads like a stuck sync.
    _ -> atTip
 where
  heading t =
    "syncing   "
      <> pad 6 (showFFloat (Just 1) (100 * ratio t) "%")
      <> "  slot "
      <> commas (unSlotNo slotNo)
      <> "/"
      <> commas (unSlotNo t)
      <> "  "
      <> commas (round rate :: Word64)
      <> " blk/s  eta "
      <> eta t
      <> counters

  atTip =
    "at tip    slot "
      <> commas (unSlotNo slotNo)
      <> "  "
      <> commas (round rate :: Word64)
      <> " blk/s"
      <> counters

  counters =
    "  blocks "
      <> commas (pgBlocks pg)
      <> "  outputs "
      <> commas (pgOutputs pg)
      <> "  spends "
      <> commas (pgSpends pg)
      <> "  elapsed "
      <> duration (now - pgStartedAt pg)

  ratio t = fromIntegral (unSlotNo slotNo) / fromIntegral (unSlotNo t) :: Double

  -- Slots per second over the heartbeat window, not blocks: the remaining
  -- distance is measured in slots, and on a chain with empty slots the two rates
  -- differ by whatever fraction of slots carry a block.
  eta t
    | slotRate <= 0 = "?"
    | otherwise = duration (fromIntegral (unSlotNo t - unSlotNo slotNo) / slotRate)

  slotRate = fromIntegral (unSlotNo slotNo - pgSlotAtReport pg) / window :: Double

  -- Guard the divisor: two blocks can share a clock reading.
  window = max 1e-6 (now - pgReportedAt pg)
  rate = fromIntegral (pgBlocks pg - pgBlocksAtReport pg) / window :: Double

-- | The closing line when a bounded sync finishes.
summarise :: IORef Progress -> IO ()
summarise ref = do
  now <- getMonotonicTime
  pg <- readIORef ref
  let secs = max 1e-6 (now - pgStartedAt pg)
  logLine
    ( "sync done  blocks "
        <> commas (pgBlocks pg)
        <> "  outputs "
        <> commas (pgOutputs pg)
        <> "  spends "
        <> commas (pgSpends pg)
        <> "  elapsed "
        <> duration secs
        <> "  avg "
        <> commas (round (fromIntegral (pgBlocks pg) / secs) :: Word64)
        <> " blk/s"
    )

-- | Emit one log line, prefixed with the wall-clock time.
--
-- A bulk sync runs for tens of minutes and its output is usually read after the
-- fact, out of a redirected file, so \"when did it slow down\" needs an absolute
-- time rather than a relative elapsed figure. Local time, second resolution:
-- enough to line an event up against @cardano-node@'s own log without being
-- noise.
logLine :: String -> IO ()
logLine msg = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  putStrLn (formatTime defaultTimeLocale "%H:%M:%S" (utcToLocalTime tz now) <> "  " <> msg)

-- | Seconds as a compact human duration: @45s@, @6m12s@, @2h04m@.
duration :: Double -> String
duration secs
  | secs < 60 = show s <> "s"
  | secs < 3600 = show m <> "m" <> pad0 (s - m * 60) <> "s"
  | otherwise = show h <> "h" <> pad0 (m - h * 60) <> "m"
 where
  s = max 0 (round secs) :: Int
  m = s `div` 60
  h = m `div` 60
  pad0 n = if n < 10 then '0' : show n else show n

-- | Thousands separators. Seven-figure block and output counts are unreadable
-- without them, and these lines exist to be skimmed.
commas :: Show a => a -> String
commas = reverse . intercalate "," . chunksOf3 . reverse . show
 where
  chunksOf3 [] = []
  chunksOf3 xs = let (a, b) = splitAt 3 xs in a : chunksOf3 b

-- | Left-pad to a fixed width so the percentage column does not jitter.
pad :: Int -> String -> String
pad n s = replicate (n - length s) ' ' <> s
