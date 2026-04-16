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
testRunTimeToSec = conjoin
      [ runTimeToSec (HMS 1 2 3) === 3723 
      , runTimeToSec ( HMS 0 60 0) === 3600
      ]

testParseTimeInvalidSeconds :: Property
testParseTimeInvalidSeconds = conjoin
      [ parseTime (pack "5:61") === Left InvalidSeconds
      -- , parseTime (pack "5:-1") === Left InvalidFormat -- is negative numbers InvalidSeconds or InvalidFormat..?
      ]
