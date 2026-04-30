{-| Module      : RunTime
Description : Represent and convert race run times

Creates data structures for run times in mm:ss or h:mm:ss format,
conversion to and from seconds, and formatted text.
-}
module RunTime where

import           Data.Text   (Text, pack)
import           Text.Printf (printf)

-- | Race run time in either minutes:seconds or hours:minutes:seconds format
--
-- Constructors:
--
-- * @MS m s@ - minutes and seconds (MS 20 30 for 20:30)
-- * @HMS h m s@ - hours, minutes, seconds (HMS 2 30 00 for 2:30:00)
data RunTime
  = MS Int Int
  | HMS Int Int Int
  deriving (Show, Eq)

-- | Convert RunTime to total seconds
--
-- Handles both MS (minutes:seconds) and HMS (hours:minutes:seconds) formats
runTimeToSec :: RunTime -> Int
runTimeToSec (MS m s)    = m * 60 + s
runTimeToSec (HMS h m s) = h * 3600 + m * 60 + s

-- | Convert total seconds to RunTime
--
-- Handles both MS (minutes:seconds) and HMS (hours:minutes:seconds) formats
secToRunTime :: Int -> RunTime
secToRunTime totalSeconds
  | hours > 0 = HMS hours mins secs
  | otherwise = MS mins secs
  where
    hours = totalSeconds `div` 3600
    mins  = (totalSeconds `mod` 3600) `div` 60
    secs  = totalSeconds `mod` 60

-- | Format Runtime as Text
--
-- Minutes and seconds are zero padded
showRunTime :: RunTime -> Text
showRunTime (MS m s)    = pack $ printf "%d:%02d" m s
showRunTime (HMS h m s) = pack $ printf "%d:%02d:%02d" h m s
