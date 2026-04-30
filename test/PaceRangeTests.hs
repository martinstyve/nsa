module PaceRangeTests (paceRangeTests) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           PaceRange

paceRangeTests :: TestTree
paceRangeTests = testGroup "PaceRange"
    [ testCase "0 seconds -> 0:00" $ toPace 0 @?= (0, 0)
    , testCase "59 seconds -> 0:59" $ toPace 59 @?= (0, 59)
    , testCase "60 seconds -> 1:00" $ toPace 60 @?= (1, 0)
    , testCase "3599 seconds -> 59:59" $ toPace 3599 @?= (59, 59)
    , QC.testProperty "toPace preserves total seconds" prop_toPace_inversion
    , QC.testProperty "toPace always returns seconds in [0, 59]" prop_toPace_valid_seconds
    , QC.testProperty "calculatePaces returns correct number of zones" prop_calculatePaces_zoneCount
  ]

prop_toPace_inversion :: Int -> Property
prop_toPace_inversion totalSec =
  let (m, s) = toPace totalSec
  in (m * 60 + s) === totalSec

prop_toPace_valid_seconds :: Int -> Property
prop_toPace_valid_seconds totalSec =
  let (_, s) = toPace totalSec
  in QC.property (s >= 0 && s < 60)

prop_calculatePaces_zoneCount :: NonNegative Double -> Property
prop_calculatePaces_zoneCount (NonNegative vdot) =
  let v = 20 + fromIntegral (floor vdot `mod` 60 :: Int)  -- constrain VDOT 20-80
      paces = calculatePaces v
      zoneCount = length [minBound .. maxBound :: Zone]
  in length paces === zoneCount
  