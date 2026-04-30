module Main where

import  ParserTests
import  PaceRangeTests
import  RunTimeTests
import  VDOTTests
import Test.Tasty

main :: IO ()
main = defaultMain tests

-- split into what module it tests
tests :: TestTree
tests = testGroup "nsa" [parserTests, paceRangeTests, runTimeTests, vdotTests]
