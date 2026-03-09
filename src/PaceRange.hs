module PaceRange where

import           VDOT

data Zone
  = ShortRep
  | MediumRep
  | LongRep
  deriving (Show, Eq, Enum, Bounded)

data PaceRange = PaceRange
  { name      :: String
  , minPace   :: Double
  , maxPace   :: Double
  , intensity :: String
  } deriving (Show, Eq)

type Pace = (Integer, Integer)

calculatePaces :: VDOT -> [PaceRange]
calculatePaces vdot = map (calculateZonePace vdot) [minBound .. maxBound]

calculateZonePace :: VDOT -> Zone -> PaceRange
calculateZonePace vdot zone =
  case zone of
    ShortRep ->
      PaceRange
        { name = "Short Length Intervals"
        , minPace = 0 -- pace per k for +/- 5% of 15k intensity
        , maxPace = 1 -- pacePerKm $ equivalentPace vdot (CustomDistance 15000)
        , intensity = "15k pace"
        }
    MediumRep ->
      PaceRange
        { name = "Medium Length Intervals"
        , minPace = 0 * vdot
        , maxPace = 1
        , intensity = "Half Marathon pace"
        }
    LongRep ->
      PaceRange
        { name = "Long Length Intervals"
        , minPace = 0 * vdot
        , maxPace = 1
        , intensity = "30k pace"
        }

-- todo : find equivalentPace from VDOT module
-- output correct pace range. tuple (min,sec)?
-- todo sort out RaceTime and other times in vdot, messy
pacePerKm :: RaceTime -> RaceDistance -> Pace
pacePerKm time distance = undefined
