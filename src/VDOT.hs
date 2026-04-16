{-# LANGUAGE OverloadedStrings #-}

module VDOT where

import           Data.Text (Text)

-- vdot formula:
-- | module comment
--
-- thanks to Larry Simpson for providing [formula](http://www.simpsonassociatesinc.com/runningmath1.htm)
-- and Daniels/Gilbert for formula
data RaceDistance
  = FifteenHundred
  | OneMile
  | ThreeK
  | FiveK
  | TenK
  | TenMile
  | HalfMarathon
  | Marathon
  | CustomDistance Double
  deriving (Show, Eq)

data RaceDistancePreset = RaceDistancePreset
  { presetValue    :: Text
  , presetLabel    :: Text
  , presetDistance :: RaceDistance
  }

presetRaceDistances :: [RaceDistancePreset]
presetRaceDistances =
  [ RaceDistancePreset "1500m" "1500 m" FifteenHundred
  , RaceDistancePreset "mile" "1 mile" OneMile
  , RaceDistancePreset "3000m" "3000 m" ThreeK
  , RaceDistancePreset "5k" "5 km" FiveK
  , RaceDistancePreset "10k" "10 km" TenK
  , RaceDistancePreset "10mile" "10 mile" TenMile
  , RaceDistancePreset "half" "Half marathon" HalfMarathon
  , RaceDistancePreset "marathon" "Marathon" Marathon
  ]

type VDOT = Double

type RaceTime = Int -- TODO: decide on Double or Integer. int most clean

distanceNumerical :: RaceDistance -> Double
distanceNumerical FifteenHundred     = 1500.0
distanceNumerical OneMile            = 1609
distanceNumerical ThreeK             = 3000.0
distanceNumerical FiveK              = 5000.0
distanceNumerical TenK               = 10000.0
distanceNumerical TenMile            = 16093
distanceNumerical Marathon           = 42195.0
distanceNumerical HalfMarathon       = 21097.5
distanceNumerical (CustomDistance n) = n

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
