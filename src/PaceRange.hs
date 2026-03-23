module PaceRange where

import           VDOT

data Zone
  = ShortRep
  | MediumRep
  | LongRep
  deriving (Show, Eq, Enum, Bounded)

data PaceRange = PaceRange
  { name      :: String
  , minPace   :: Integer -- sec per k
  , maxPace   :: Integer -- sec per k
  , intensity :: String
  } deriving (Show, Eq)

type Pace = (Integer, Integer)

calculatePaces :: VDOT -> [PaceRange]
calculatePaces vdot = map (calculateZonePace vdot) [minBound .. maxBound]

-- todo: find good pace ranges. this is manually adjusted to match lactrace
-- at 18:30 5k
calculateZonePace :: VDOT -> Zone -> PaceRange
calculateZonePace vdot zone =
  case zone of
      ShortRep ->
        PaceRange "Short Intervals" 
                  (paceAtDistance vdot 15000)
                  (paceAtDistance vdot 23000)
                  "15k intensity"
      MediumRep ->
        PaceRange "Medium Intervals"
                  (paceAtDistance vdot 21000)
                  (paceAtDistance vdot 33000)
                  "HM intensity"
      LongRep ->
        PaceRange "Long Intervals"
                  (paceAtDistance vdot 30000)
                  (paceAtDistance vdot 50000)
                  "30k intensity"

paceAtDistance :: VDOT -> Double -> Integer
paceAtDistance vdot d = fst $ pacePerKm (equivalentTime vdot (CustomDistance d)) (CustomDistance d)

pacePerKm :: RaceTime -> RaceDistance -> Pace
pacePerKm time distance = (secondsPerKm, secondsPerKm)
  where
    meters = distanceNumerical distance
    secondsPerKm
      | meters <= 0 = 0
      | otherwise = round (fromIntegral time * 1000 / meters)
