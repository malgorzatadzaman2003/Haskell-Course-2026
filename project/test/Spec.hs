module Main where

import Test.Hspec
import SpreadsheetLang.AST
import SpreadsheetLang.Parser
import Text.Megaparsec

main :: IO ()
main = hspec $ do
  describe "AST" $ do
    it "creates a simple numeric cell" $ do
      let cell = Cell ("A", 1) (Lit (NumV 10))
      cell `shouldBe` Cell ("A", 1) (Lit (NumV 10))

    describe "Parser" $ do
        it "parses simple numeric cell" $ do
            parse parseCell "" "A1 = 10;"
                `shouldBe`
                Right
                (Cell ("A",1)
                        (Lit (NumV 10)))