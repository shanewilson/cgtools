module CGTools.Install.Test (installSuite) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
-- import CGTools.Install.Internal (genHookPath)

installSuite :: TestTree
installSuite = testGroup "Install"
    [testCase "getPrepareCommitPath" testGenHookPath]

testGenHookPath :: Assertion
testGenHookPath = "gitdir/hooks/prepare-commit-msg" @=? "gitdir"