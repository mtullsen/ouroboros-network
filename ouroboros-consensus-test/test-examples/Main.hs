module Main (main) where

import           Test.Tasty

import           Test.Example (test1)

main :: IO ()
main = defaultMain test

test :: TestTree
test =
  testGroup "ouroboros-examples"
  [ test1
  ]

