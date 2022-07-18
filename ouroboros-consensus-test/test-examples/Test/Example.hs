module Test.Example
  ( test1
  )
  where

import           Test.Tasty
import           Test.Tasty.QuickCheck


test1 = testProperty "prop_example1" prop_example1

prop_example1 :: Bool
prop_example1 = True

