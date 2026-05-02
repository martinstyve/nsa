module RunTimeTests (runTimeTests) where

import           Data.Text             (pack)
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import           Test.Tasty.QuickCheck as QC (NonNegative (NonNegative),
                                              Property, testProperty, (===))

import           RunTime               as RT (runTimeToSec, secToRunTime, showRunTime)

runTimeTests :: TestTree
runTimeTests = testGroup "RunTime"
  [ testCase "showRunTime formats sub one minute" $ RT.showRunTime (secToRunTime 59) @?= pack "0:59"
  , testCase "showRunTime formats one hour" $ RT.showRunTime (secToRunTime 3600) @?= pack "1:00:00"
  , QC.testProperty "seconds to runtime inversion confirmation" prop_secondsToRunTime_Inversion
  ]

prop_secondsToRunTime_Inversion :: NonNegative Int -> Property
prop_secondsToRunTime_Inversion (NonNegative totalSeconds) =
  runTimeToSec (secToRunTime totalSeconds) === totalSeconds
