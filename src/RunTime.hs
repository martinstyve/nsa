module RunTime where

import           Data.Text   (Text, pack)
import           Text.Printf (printf)

data RunTime
  = MS Int Int -- mm:ss
  | HMS Int Int Int -- h:mm:ss
  deriving (Show, Eq)

runTimeToSec :: RunTime -> Int
runTimeToSec (MS m s)    = m * 60 + s
runTimeToSec (HMS h m s) = h * 3600 + m * 60 + s

formatRunTime :: Int -> Text
formatRunTime totalSeconds = pack $
  if hours > 0
    then printf "%d:%02d:%02d" hours mins secs
    else printf "%d:%02d" mins secs
  where
    hours = totalSeconds `div` 3600
    mins = (totalSeconds `mod` 3600) `div` 60
    secs = totalSeconds `mod` 60
