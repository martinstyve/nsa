{-| Module      : PaceRange
Description : Calculate training pace ranges for different workout intensities

Finds best paces for interval training based on vdot
Splits training into three zones (short, medium, long repetitions) with pace ranges
-}
module PaceRange where

import           RaceDistance (RaceDistance (CustomDistance), distanceNumerical)
import           VDOT         (VDOT, equivalentTime)

-- | Training zone classification
--
-- Defines three training zones based on distance and intensity:
--
-- * @ShortRep@ - Short intervals (15k intensity)
-- * @MediumRep@ - Medium intervals (HM intensity)
-- * @LongRep@ - Long intervals (30k intensity)
data Zone
  = ShortRep
  | MediumRep
  | LongRep
  deriving (Show, Eq, Enum, Bounded)

-- | Training pace range for a specific intensity zone
--
-- Fields:
--
-- * @name@ - Workout zone name
-- * @minPace@ - Minimum pace in seconds per kilometer
-- * @maxPace@ - Maximum pace in seconds per kilometer
-- * @intensity@ - Reference intensity level
data PaceRange = PaceRange
  { name      :: String
  , minPace   :: Int -- sec per k
  , maxPace   :: Int -- sec per k
  , intensity :: String
  } deriving (Show, Eq)

-- | (minutes, seconds)
type Pace = (Int, Int)

-- | Convert seconds to (minutes, seconds)
toPace :: Int -> Pace
toPace totalSec = (totalSec `div` 60, totalSec `mod` 60)

-- | Calculate pace in seconds per kilometer
--
-- Parameters: @time@ in seconds, @distance@ as RaceDistance
--
-- Returns pace as seconds per km, or 0 if distance is invalid
pacePerKm :: Int -> RaceDistance -> Int
pacePerKm time d
  | meters <= 0 = 0
  | otherwise = round (fromIntegral time * 1000 / meters)
  where
    meters = distanceNumerical d

-- | Calculate pace at a specific distance based on VDOT
--
-- Parameters: @vdot@, @d@ (distance in meters)
--
-- Returns 0 if the distance is out of set range, otherwise pace in seconds per km
paceAtDistance :: VDOT -> Double -> Int
paceAtDistance vdot d =
  either (const 0) paceCalc (equivalentTime vdot (CustomDistance d))
  where
    paceCalc time = pacePerKm time (CustomDistance d)

-- | Calculate all training pace ranges for a given vdot
--
-- Returns a list of pace ranges
calculatePaces :: VDOT -> [PaceRange]
calculatePaces vdot = map (calculateZonePace vdot) [minBound .. maxBound]

-- | Calculate pace range for a specific training zone
--
-- Decides min/max pace by calculating equivalent times at other distances
--
-- Returns a pace range with description for a given intensity zone
calculateZonePace :: VDOT -> Zone -> PaceRange
calculateZonePace vdot zone =
  case zone of
    ShortRep -> PaceRange
        { name = "Short Intervals"
        , minPace = paceAtDistance vdot 15000
        , maxPace = paceAtDistance vdot 23000
        , intensity = "15k intensity" }
    MediumRep -> PaceRange
        { name = "Medium Intervals"
        , minPace = paceAtDistance vdot 21000
        , maxPace = paceAtDistance vdot 33000
        , intensity = "HM intensity" }
    LongRep -> PaceRange
        { name = "Long Intervals"
        , minPace = paceAtDistance vdot 30000
        , maxPace = paceAtDistance vdot 42195
        , intensity = "30k intensity" }
