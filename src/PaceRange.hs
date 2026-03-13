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

-- todo: find good pace ranges
calculateZonePace :: VDOT -> Zone -> PaceRange
calculateZonePace vdot zone =
  case zone of
      ShortRep ->
        PaceRange "Short Intervals" 
                  (equivalentTime (vdot * 1.05) (CustomDistance 1000))
                  (equivalentTime (vdot * 0.95) (CustomDistance 1000))
                  "15k intensity"
      MediumRep ->
        PaceRange "Medium Intervals"
                  (equivalentTime vdot (CustomDistance 1000))
                  (equivalentTime (vdot * 0.90) (CustomDistance 1000))
                  "HM intensity"
      LongRep ->
        PaceRange "Long Intervals"
                  (equivalentTime (vdot * 0.85) (CustomDistance 1000))
                  (equivalentTime (vdot * 0.80) (CustomDistance 1000))
                  "30k intensity"

-- pacePerKm :: RaceTime -> RaceDistance -> Pace
-- pacePerKm time distance = undefined
