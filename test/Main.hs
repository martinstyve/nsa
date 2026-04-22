module Main where

import           Data.Text             (pack)
import           Parser
import           RunTime
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           PaceRange
import           VDOT

main :: IO ()
main = defaultMain tests

-- split into what module it tests
tests :: TestTree
tests = testGroup "nsa" [parserTests, paceRangeTests, runTimeTests, vdotTests]

-- first unit tests then property
parserTests :: TestTree
parserTests = testGroup "Parser"
  [ testCase "parseTime rejects invalid seconds" $ parseTime (pack "5:61") @?= Left InvalidSeconds
  , QC.testProperty "formatRunTime round-trips through parseTime" prop_formatRunTimeRoundTrip
  ]

-- first unit tests then property
paceRangeTests :: TestTree
paceRangeTests = testGroup "PaceRange"
  [ testGroup "toPace"
    [ testCase "0 seconds -> 0:00" $ toPace 0 @?= (0, 0)
    , testCase "59 seconds -> 0:59" $ toPace 59 @?= (0, 59)
    , testCase "60 seconds -> 1:00" $ toPace 60 @?= (1, 0)
    , testCase "3599 seconds -> 59:59" $ toPace 3599 @?= (59, 59) ]
  , QC.testProperty "toPace preserves total seconds" prop_toPace_inversion
  , QC.testProperty "toPace always returns seconds in [0, 59]" prop_toPace_valid_seconds
  , QC.testProperty "calculatePaces returns correct number of zones" prop_calculatePaces_count
  , QC.testProperty "calculatePaces minPace <= maxPace" prop_calculatePaces_ordered
  ]

-- first unit tests then property todo: unit
runTimeTests :: TestTree
runTimeTests = testGroup "RunTime"
  [ QC.testProperty "seconds to runtime inversion confirmation" prop_secondsToRunTime_Inversion
  , QC.testProperty "formatRunTime . runTimeToSec round-trip" prop_formatRunTimeRoundTrip_composition
  ]

vdotTests :: TestTree
vdotTests = testGroup "VDOT"
  [ testCase "equivalentTime vdot inversion at 5k" $
      let time = equivalentTime 67 FiveK -- 67 is VDOT number
          calculatedVdot = calculateVDOT (fromIntegral time) FiveK
      in abs (calculatedVdot - 67) < 0.1 @?= True -- epsilon needed
  ]


--------------------    Parser      --------------------
prop_formatRunTimeRoundTrip :: NonNegative Int -> Property
prop_formatRunTimeRoundTrip (NonNegative totalSeconds) =
  parseTime (formatRunTime totalSeconds) === Right (secondsToRunTime totalSeconds)


--------------------   PaceRange    --------------------
prop_toPace_inversion :: Int -> Property
prop_toPace_inversion totalSec =
  let (m, s) = toPace totalSec
  in (m * 60 + s) === totalSec

prop_toPace_valid_seconds :: Int -> Property
prop_toPace_valid_seconds totalSec =
  let (_, s) = toPace totalSec
  in QC.property (s >= 0 && s < 60)

prop_calculatePaces_count :: NonNegative Double -> Property
prop_calculatePaces_count (NonNegative vdot) =
  let v = 20 + fromIntegral (floor vdot `mod` 60 :: Int)  -- constrain VDOT 20-80
      paces = calculatePaces v
      zoneCount = length [minBound .. maxBound :: Zone]
  in length paces === zoneCount

prop_calculatePaces_ordered :: NonNegative Double -> Property
prop_calculatePaces_ordered (NonNegative vdot) =
  let v = 20 + fromIntegral (floor vdot `mod` 60 :: Int)  -- constrain to 20-80
  in QC.property (all (\p -> minPace p <= maxPace p) (calculatePaces v))


--------------------    RunTime     --------------------
-- easy test for helper
-- totalsec -> runtime -> totalsec
prop_secondsToRunTime_Inversion :: NonNegative Int -> Property
prop_secondsToRunTime_Inversion (NonNegative totalSeconds) =
  runTimeToSec (secondsToRunTime totalSeconds) === totalSeconds

prop_formatRunTimeRoundTrip_composition :: NonNegative Int -> Property
prop_formatRunTimeRoundTrip_composition (NonNegative totalSeconds) =
  let rt = secondsToRunTime totalSeconds
      formatted = formatRunTime (runTimeToSec rt)
  in formatted === formatRunTime totalSeconds

--------------------    helpers     --------------------
secondsToRunTime :: Int -> RunTime
secondsToRunTime totalSec
  | totalSec < 3600 = MS m s
  | otherwise = HMS h m s
  where
    h = totalSec `div` 3600
    m = (totalSec `mod` 3600) `div` 60
    s = totalSec `mod` 60
