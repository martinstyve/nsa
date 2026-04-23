module VDOT where

-- vdot formula:
-- | module comment
--
-- thanks to Larry Simpson for providing [formula](http://www.simpsonassociatesinc.com/runningmath1.htm)
-- and Daniels/Gilbert for formula
import           RaceDistance

type VDOT = Double

-- | formula found through link at top of module
-- oxygen cost formula on page 2
-- drop dead formula on page 3
-- "max intensity" divided by "duration human can run at intensity"
-- velocity v is expressed as meters per minute
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

-- todo generic num
bisect :: (Double -> Double) -> Double -> Double -> Double -> Double
bisect f target low high
  | (high - low) < 0.01 = mid
  | f mid > target = bisect f target mid high
  | otherwise = bisect f target low mid
  where
    mid = (low + high) / 2

-- | calculate other times from a given VDOT, but we dont have a formula or table
-- to look up what is equivalent, so need to search through vdots until we get
-- a distance+time which gives an equal vdot (0.01 margin)
equivalentTime :: VDOT -> RaceDistance -> Int
equivalentTime vdot distance
  -- range now 1 second to 24 hours
  -- expected use for calculator is 1500 to marathon. Should it be hard limit
  -- on 1500 WR to normal marathon cut-off time?
  -- update TODO: set hard limit WR 800m to WR marathon
 = round $ bisect (`calculateVDOT` distance) vdot 1 (24 * 3600)
