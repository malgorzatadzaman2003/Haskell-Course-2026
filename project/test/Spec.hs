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

        it "parses a sheet with multiple cells" $ do
            let input =
                    "sheet {\n\
                    \  A1 = 10;\n\
                    \  A2 = 20;\n\
                    \}"
            parse parseSheet "" input
                `shouldBe`
                Right
                (Sheet [ Cell ("A",1) (Lit (NumV 10))
                       , Cell ("A",2) (Lit (NumV 20))
                       ])   