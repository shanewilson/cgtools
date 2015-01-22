module CGTools.Install.Test (installSuite) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import CGTools.Install.Internal (getWithDefault)

installSuite :: TestTree
installSuite = testGroup "Install"
    [testCase "testGetWithDefaultWithDefault" testGetWithDefaultWithDefault,
     testCase "testGetWithDefaultNoDefault" testGetWithDefaultNoDefault]

testGetWithDefaultWithDefault :: Assertion
testGetWithDefaultWithDefault = "my_value" @=? getWithDefault "my_value" "my_default"

testGetWithDefaultNoDefault :: Assertion
testGetWithDefaultNoDefault = "my_default" @=? getWithDefault "" "my_default"