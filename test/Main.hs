module Main where

import Parser
import RunTime
import Data.Text (pack)
import Test.QuickCheck

main :: IO ()
main = do
      quickCheck testRunTimeToSec
      quickCheck testParseTimeInvalidSeconds

testRunTimeToSec :: Property
testRunTimeToSec =
      runTimeToSec (HMS 1 2 3) === 3723

testParseTimeInvalidSeconds :: Property
testParseTimeInvalidSeconds =
      parseTime (pack "5:61") === Left InvalidSeconds
