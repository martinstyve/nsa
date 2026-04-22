module Main where

import           Data.Text             (pack)
import           Parser
import           RunTime
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import PaceRange

main :: IO ()
main = defaultMain tests

-- split into what module it tests
tests :: TestTree
tests = testGroup "nsa" [parserTests, paceRangeTests, runTimeTests]

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
  ]

-- first unit tests then property todo: unit
runTimeTests :: TestTree
runTimeTests = testGroup "RunTime"
  [ QC.testProperty "seconds to runtime inversion confirmation" prop_secondsToRunTime_Inversion ]

-------------------- Property tests --------------------
prop_formatRunTimeRoundTrip :: NonNegative Int -> Property
prop_formatRunTimeRoundTrip (NonNegative totalSeconds) =
  parseTime (formatRunTime totalSeconds) === Right (secondsToRunTime totalSeconds)

-- easy test for helper
-- totalsec -> runtime -> totalsec
prop_secondsToRunTime_Inversion :: NonNegative Int -> Property
prop_secondsToRunTime_Inversion (NonNegative totalSeconds) =
  runTimeToSec (secondsToRunTime totalSeconds) === totalSeconds

prop_toPace_inversion :: Int -> Property
prop_toPace_inversion totalSec =
  let (m, s) = toPace totalSec
  in (m * 60 + s) === totalSec

prop_toPace_valid_seconds :: Int -> Property
prop_toPace_valid_seconds totalSec =
  let (_, s) = toPace totalSec
  in QC.property (s >= 0 && s < 60)
  
--------------------    helpers     --------------------
secondsToRunTime :: Int -> RunTime
secondsToRunTime totalSec
  | totalSec < 3600 = MS m s
  | otherwise = HMS h m s
  where
    h = totalSec `div` 3600
    m = (totalSec `mod` 3600) `div` 60
    s = totalSec `mod` 60
