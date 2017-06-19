{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Sajson
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "sajsonParse" $ do
    it "returns True for correct JSON document" $ do
      r <- sajsonParse "{\"abc\":123,\"def\":[1,2,3]}"
      r `shouldBe` True
    it "returns False for truncated JSON document" $ do
      r <- sajsonParse "{\"abc\":123,\"def\":[1,2,3"
      r `shouldBe` False
