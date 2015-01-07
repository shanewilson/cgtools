module Main where

import Test.Tasty (defaultMain,testGroup,TestTree)

import CGTools.Validate.Test (validateSuite)

tests :: TestTree
tests = testGroup "All Tests" [ validateSuite ]

main :: IO ()
main = defaultMain tests