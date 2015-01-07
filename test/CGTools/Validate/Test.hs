module CGTools.Validate.Test ( validateSuite ) where

import              Data.Char                       (isPrint,isSymbol)
import              Test.Tasty                      (testGroup, TestTree)
import              Test.Tasty.HUnit
import              Test.Tasty.SmallCheck           (forAll)
import qualified    Test.Tasty.SmallCheck       as  SC
import qualified    Test.Tasty.QuickCheck       as  QC
import              Test.SmallCheck.Series          (Serial)
import              CGTools.Validate            as  V

-- | Test with QuickCheck and SmallCheck
tp name prop = testGroup name
    [ QC.testProperty "QC" prop
    , SC.testProperty "SC" prop
    ]

validateSuite :: TestTree
validateSuite = testGroup "Validate"
    [ tp "pass" $ True
    , tp "fail" $ False
    , testCase "unit" $ V.dropFromEndWhile even [1,2,3,4,4,4,4,4] @?= [1,2,3]]

-- import Data.List
-- import Data.Ord
--
-- validateSuite :: TestTree
-- validateSuite = testGroup "Tests" [properties, unitTests]
--
-- properties :: TestTree
-- properties = testGroup "Properties" [scProps, qcProps]
--
-- scProps = testGroup "(checked by SmallCheck)"
--   [ SC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , SC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , SC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
--   ]
--
-- qcProps = testGroup "(checked by QuickCheck)"
--   [ QC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , QC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , QC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
--   ]
--
-- unitTests = testGroup "Unit tests"
--   [ testCase "List comparison (different length)" $
--       [1, 2, 3] `compare` [1,2] @?= GT
--
--   -- the following test does not hold
--   , testCase "List comparison (same length)" $
--       [1, 2, 3] `compare` [1,2,2] @?= LT
--   ]
