module Control.Foldl.ExtraSpec
  ( spec
  ) where

import Control.Foldl.Extra

import Control.Foldl (fold)
import Data.Map (Map)
import Data.Set (Set)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "collectUnique" $ do
    let example :: [(Int, String)]
                -> Either (Set Int) (Map Int String)
                -> Expectation
        example input output =
          fold collectUnique input `shouldBe` output

    it "should succeed on empty input" $
      example [] (Right Map.empty)

    it "should succeed on input without duplicates" $
      example [(1, "A"), (2, "B"), (3, "C")]
              (Right [(1, "A"), (2, "B"), (3, "C")])

    it "should fail on input with duplicates" $
      example [(1, "A"), (1, "B"), (2, "C")]
              (Left [1])
