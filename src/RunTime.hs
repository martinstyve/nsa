module RunTime where
import Data.Text (Text, pack)

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
    then show hours ++ ":" ++ zeroPad mins ++ ":" ++ zeroPad secs
    else zeroPad mins ++ ":" ++ zeroPad secs
  where
    hours = totalSeconds `div` 3600
    mins = (totalSeconds `mod` 3600) `div` 60
    secs = totalSeconds `mod` 60
    zeroPad n =
      if n < 10
        then "0" ++ show n
        else show n
