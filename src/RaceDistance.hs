{-# LANGUAGE OverloadedStrings #-}

module RaceDistance where

import           Data.Text (Text)

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
