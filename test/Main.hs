module Main where

import AppTests (appTests)
import ParserTests (parserTests)
import PaceRangeTests (paceRangeTests)
import RunTimeTests (runTimeTests)
import VDOTTests (vdotTests)
import Test.Tasty (defaultMain, testGroup, TestTree)

main :: IO ()
main = defaultMain tests

-- split into what module it tests
tests :: TestTree
tests = testGroup "nsa" [appTests, parserTests, paceRangeTests, runTimeTests, vdotTests]
