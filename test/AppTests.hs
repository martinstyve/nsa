module AppTests (appTests) where

import           Data.Text                  (pack)
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (assertFailure, testCase, (@?=))

import           App
import           Parser                     (InputError (InvalidFormat))
import           RaceDistance               (RaceDistance (FiveK))
import           RunTime                    (RunTime (MS))

appTests :: TestTree
appTests = testGroup "AppHtml"
  [ testCase "appErrorText hides missing required input" $ appErrorText MissingRequiredInput @?= Nothing
  , testCase "appErrorText exposes parser errors" $ appErrorText (InputParseError InvalidFormat) @?= Just (pack "use format h:mm:ss or mm:ss")
  , testCase "validateParams accepts complete request data"
        (case validateParams (Just (TimeParam (pack "25:30"))) (Just (DistanceParam (pack "5k"))) Nothing of
           Left _      -> assertFailure "expected Right value"
           Right value -> value @?= (MS 25 30, FiveK))
  ]
