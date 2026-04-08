module PaceRange where

import           VDOT

data Zone
  = ShortRep
  | MediumRep
  | LongRep
  deriving (Show, Eq, Enum, Bounded)

data PaceRange = PaceRange
  { name      :: String
  , minPace   :: Int -- sec per k
  , maxPace   :: Int -- sec per k
  , intensity :: String
  } deriving (Show, Eq)

type Pace = (Int, Int) -- (Minutes, Seconds)

toPace :: Int -> Pace
toPace totalSec = (totalSec `div` 60, totalSec `mod` 60)

pacePerKm :: Int -> RaceDistance -> Int
pacePerKm time distance
  | meters <= 0 = 0
  | otherwise = round (fromIntegral time * 1000 / meters)
  where
    meters = distanceNumerical distance

paceAtDistance :: VDOT -> Double -> Int
paceAtDistance vdot d =
  let dist = CustomDistance d
      time = equivalentTime vdot dist
   in pacePerKm time dist

calculatePaces :: VDOT -> [PaceRange]
calculatePaces vdot = map (calculateZonePace vdot) [minBound .. maxBound]

-- todo: find good pace ranges. this is manually adjusted to match lactrace
-- at 18:30 5k
calculateZonePace :: VDOT -> Zone -> PaceRange
calculateZonePace vdot zone =
  case zone of
    ShortRep ->
      PaceRange
        { name = "Short Intervals"
        , minPace = paceAtDistance vdot 15000
        , maxPace = paceAtDistance vdot 23000
        , intensity = "15k intensity"
        }
    MediumRep ->
      PaceRange
        { name = "Medium Intervals"
        , minPace = paceAtDistance vdot 21000
        , maxPace = paceAtDistance vdot 33000
        , intensity = "HM intensity"
        }
    LongRep ->
      PaceRange
        { name = "Long Intervals"
        , minPace = paceAtDistance vdot 30000
        , maxPace = paceAtDistance vdot 50000
        , intensity = "30k intensity"
        }
