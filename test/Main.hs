module Main where

import ParserTests (parserTests)
import PaceRangeTests (paceRangeTests)
import RunTimeTests (runTimeTests)
import VDOTTests (vdotTests)
import Test.Tasty (defaultMain, testGroup, TestTree)

main :: IO ()
main = defaultMain tests

-- split into what module it tests
tests :: TestTree
tests = testGroup "nsa" [parserTests, paceRangeTests, runTimeTests, vdotTests]
