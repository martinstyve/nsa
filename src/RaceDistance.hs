{-# LANGUAGE OverloadedStrings #-}

{-| Module      : RaceDistance
Description : Represent and convert common race distances

Provides standard running race distances from 1500m to marathon,
with preset labels for UI display and conversion to meters
-}
module RaceDistance where

import           Data.Text (Text)

-- | Standard race distances
--
-- Common track and road race distances, with a CustomDistance constructor
-- for distances in meters
data RaceDistance
  = FifteenHundred
  | OneMile
  | ThreeK
  | FiveK
  | TenK
  | TenMile
  | HalfMarathon
  | Marathon
  | CustomDistance Double -- in meters
  deriving (Show, Eq)

-- | UI preset for a race distance
--
-- Fields:
--
-- * @presetValue@ - For computer to read
-- * @presetLabel@ - For humans to read
-- * @presetDistance@ - Actual data type
data RaceDistancePreset = RaceDistancePreset
  { presetValue    :: Text
  , presetLabel    :: Text
  , presetDistance :: RaceDistance
  }

-- | List of standard race distance presets
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

-- | Convert race distance to meters
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
