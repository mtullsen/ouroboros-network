module Main (main) where

import           Test.Tasty

import           Test.Example


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-examples"
  [ return True -- ??
  ]
