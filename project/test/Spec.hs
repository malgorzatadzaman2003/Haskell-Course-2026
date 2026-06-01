module Main where

import Test.Hspec
import SpreadsheetLang.AST

main :: IO ()
main = hspec $ do
  describe "AST" $ do
    it "creates a simple numeric cell" $ do
      let cell = Cell ("A", 1) (Lit (NumV 10))
      cell `shouldBe` Cell ("A", 1) (Lit (NumV 10))