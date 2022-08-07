module Main (main) where

import           Test.Tasty

import           Test.Tutorial
import           Test.Example
import           Test.ToyLedger1

main :: IO ()
main = defaultMain test

{-
FIXME: add some [quickcheck] tests, ideally in separate modules.

test :: TestTree
test =
  testGroup "ouroboros-examples"
  [ test1
  ]
-}


