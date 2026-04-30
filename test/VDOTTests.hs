module VDOTTests (vdotTests) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           RaceDistance
import           VDOT

vdotTests :: TestTree
vdotTests = testGroup "VDOT"
  [ testCase "distanceNumerical custom distance works" $ distanceNumerical (CustomDistance 6767.6) @?= 6767.6
  , testCase "equivalentTime vdot inversion at 5k" $
      case equivalentTime 67 FiveK of
        Left err -> assertFailure $ "expected Right value. got: " ++ show err
        Right time ->
          let calculatedVdot = calculateVDOT (fromIntegral time) FiveK
          in abs (calculatedVdot - 67) < 0.1 @?= True -- epsilon needed
  ]
  