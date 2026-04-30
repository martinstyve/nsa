module ParserTests (parserTests) where

import           Data.Text             (pack)
import           Parser
import           RunTime as RT
import           RaceDistance
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

parserTests :: TestTree
parserTests = testGroup "Parser"
  [ testCase "parseTime accepts mm:ss upper bound" $ parseTime (pack "5:59") @?= Right (MS 5 59)
  , testCase "parseTime rejects invalid seconds" $ parseTime (pack "5:61") @?= Left InvalidSeconds
  , testCase "parseTime rejects invalid minutes in h:mm:ss" $ parseTime (pack "1:60:00") @?= Left InvalidMinutes
  , testCase "parseTime rejects too many time parts" $ parseTime (pack "1:02:03:04") @?= Left InvalidFormat
  , testCase "parseTime rejects non-number text" $ parseTime (pack "abc") @?= Left InvalidFormat
  , testCase "parseDistance accepts known value" $ parseDistance (pack "5k") @?= Right FiveK
  , testCase "parseDistance rejects unknown value" $ parseDistance (pack "7k") @?= Left InvalidDistance
  , testCase "parseCustomDistance accepts lower bound" $ parseCustomDistance (pack "1") @?= Right 1
  , testCase "parseCustomDistance rejects zero" $ parseCustomDistance (pack "0") @?= Left InvalidCustomDistance
  , testCase "parseCustomDistance rejects values above max" $ parseCustomDistance (pack "50001") @?= Left InvalidCustomDistance
  , testCase "resolveDistanceSelection rejects missing custom value" $ resolveDistanceSelection (pack "custom") Nothing @?= Left MissingCustomDistance
  , testCase "resolveDistanceSelection accepts custom value" $ resolveDistanceSelection (pack "custom") (Just (pack "4200")) @?= Right (CustomDistance 4200)
  , QC.testProperty "formatRunTime round-trips through parseTime" prop_formatRunTimeRoundTrip
  ]

prop_formatRunTimeRoundTrip :: NonNegative Int -> Property
prop_formatRunTimeRoundTrip (NonNegative totalSeconds) =
  parseTime (RT.showRunTime (secToRunTime totalSeconds)) === Right (secToRunTime totalSeconds)
  