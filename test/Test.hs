module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)

import CGTools.Install.Test (installSuite)
-- import CGTools.Validate.Test (validateSuite)

tests :: TestTree
tests = testGroup "All Tests" [  ]

main :: IO ()
main = defaultMain tests