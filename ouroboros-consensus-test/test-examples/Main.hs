module Main (main) where

import qualified System.Directory as Dir
import           System.IO.Temp
import           Test.Tasty

import           Test.Example


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-examples"
  [ return True -- ??
  ]
