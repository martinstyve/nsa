{-| Module      : VDOT
Description : Calculate VDOT (VO2 max) from race times and predict equivalent performances

VDOT represents aerobic fitness as maximum oxygen utilization (ml/kg/min)
It is calculated from race performance and can predict times for other distances

Formulas based on Jack Daniels' running research via [Larry Simpson's implementation](http://www.simpsonassociatesinc.com/runningmath1.htm)
-}
module VDOT where

import           Data.Text (Text, pack)
import           RaceDistance

-- | VO2 max equivalent value in ml/kg/minute
type VDOT = Double

-- | VDOT calculation errors
data VDOTError
  = TimeOutOfRange
  deriving (Show, Eq, Ord)

-- | Convert VDOT error to user-friendly text
vdotErrorText :: VDOTError -> Text
vdotErrorText TimeOutOfRange =
  pack "Predicted time is outside the range of this calculator (VDOT equivalent of 3:20 1500m or slower than 7 hour marathon)"

-- | Calculate VDOT from a race time and distance
--
-- Parameters: @time@ in seconds, @distance@ as RaceDistance
--
-- Formula: @VO2 cost = 0.182258 * v + 0.000104 * v^2 - 4.60@
-- where @v@ is velocity (m/min), adjusted by duration factor
calculateVDOT :: Double -> RaceDistance -> VDOT
calculateVDOT time distance = o2cost / dropDead
  where
    t = time / 60
    v = distanceNumerical distance / t
    o2cost = 0.182258 * v + 0.000104 * (v ** 2) - 4.60
    dropDead =
      0.2989558 * exp (-(0.1932605 * t))
        + 0.1894393 * exp (-(0.012778 * t))
        + 0.8

-- | Binary search to find where a function reaches a target value
--
-- Converges until interval is less than 0.01 wide
--
-- Parameters: @f@ (function), @target@ (goal value), @low@ and @high@ bounds
bisect :: (Double -> Double) -> Double -> Double -> Double -> Double
bisect f target low high
  | (high - low) < 0.01 = mid
  | f mid > target = bisect f target mid high
  | otherwise = bisect f target low mid
  where
    mid = (low + high) / 2

-- | Predict race time for a given VDOT and distance
--
-- Given VDOT and target distance, computes time in sec
--
-- Uses 'bisect' to invert 'calculateVDOT' since no closed-form inverse exists
--
-- Search range: VDOT equivalent of 00:03:20 1500m and 07:00:00 marathon
--
-- Returns an error if the result is outside the valid range
equivalentTime :: VDOT -> RaceDistance -> Either VDOTError Int
equivalentTime vdot distance
  | roundedTime < minLimit = Left TimeOutOfRange
  | roundedTime > maxLimit = Left TimeOutOfRange
  | otherwise              = Right roundedTime
  where
    calculatedTime = bisect (`calculateVDOT` distance) vdot 199 25201
    roundedTime    = round calculatedTime
    minLimit = 200        -- 00:03:20
    maxLimit = 25200      -- 07:00:00
