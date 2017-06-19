{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Sajson
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "sajsonParse" $ do
    it "returns Right for correct JSON document" $ do
      r <- sajsonParse "{\"abc\":123,\"def\":[1,2,3]}"
      r `shouldBe` (Right ())
    it "returns proper error for truncated JSON document" $ do
      r <- sajsonParse "{\"abc\":123,\"def\":[1,2,3"
      r `shouldBe` (Left (SajsonParseError {sajsonParseErrorLine = 1, sajsonParseErrorColumn = 24, sajsonParseErrorMessage = "unexpected end of input"}))
