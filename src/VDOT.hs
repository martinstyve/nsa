
-- vdot formula:
-- | module comment
--
-- thanks to Larry Simpson for providing [formula](http://www.simpsonassociatesinc.com/runningmath1.htm)
-- and Daniels/Gilbert for formula
module VDOT where

data RaceDistance
  = FiveK
  | TenK
  | HalfMarathon
  | Marathon
  | CustomDistance Double
  deriving (Show, Eq)

type VDOT = Double

type RaceTime = Integer -- TODO: decide on Double or Integer. int most clean

distanceNumerical :: RaceDistance -> Double
distanceNumerical FiveK              = 5000
distanceNumerical TenK               = 10000
distanceNumerical Marathon           = 42195
distanceNumerical HalfMarathon       = distanceNumerical Marathon / 2
distanceNumerical (CustomDistance n) = n

-- temporary (i hope)
-- (hours, minutes, seconds)
-- secToHMS :: Integer -> (Integer, Integer, Integer)
-- secToHMS n = (h, m, s)
--   where
--     h = n `div` 3600
--     m = n `mod` 3600 `div` 60
--     s = n `mod` 60

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
equivalentTime :: VDOT -> RaceDistance -> Integer
equivalentTime vdot distance
  -- range now 1 second to 24 hours
  -- expected use for calculator is 1500 to marathon. Should it be hard limit
  -- on 1500 WR to normal marathon cut-off time?
 = round $ bisect (`calculateVDOT` distance) vdot 1 (24 * 3600)
