module Main where

import SpreadsheetLang.AST
import SpreadsheetLang.Parser
import SpreadsheetLang.Evaluator
import SpreadsheetLang.Dependency
import SpreadsheetLang.CycleDetection

import Test.Hspec
import Text.Megaparsec
import qualified Data.Map as Map

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

        it "parses formula with cell references" $ do
          parse parseCell "" "A3 = A1 + A2;"
            `shouldBe`
            Right
                (Cell ("A",3)
                    (Form
                    (BinOp Add
                        (Ref ("A",1))
                        (Ref ("A",2)))))

        it "respects operator precedence" $ do
          parse parseCell "" "A4 = A1 + A2 * 2;"
            `shouldBe`
            Right
              (Cell ("A",4)
                (Form
                  (BinOp Add
                    (Ref ("A",1))
                    (BinOp Mul
                      (Ref ("A",2))
                      (LitE (NumV 2))))))

    describe "Evaluator" $ do
        it "evaluates a sheet with only literals" $ do
            let sheet = Sheet [ Cell ("A",1) (Lit (NumV 10))
                              , Cell ("A",2) (Lit (NumV 20))
                              ]
            evaluateSheet sheet
                `shouldBe`
                Map.fromList [ (("A",1), NumV 10)
                             , (("A",2), NumV 20)
                             ]
        it "evaluates formulas with references" $ do
            let sheet = Sheet [ Cell ("A",1) (Lit (NumV 10))
                              , Cell ("A",2) (Lit (NumV 20))
                              , Cell ("A",3)
                                (Form
                                    (BinOp Add
                                        (Ref ("A",1))
                                        (Ref ("A",2))))
                            ]

            evaluateSheet sheet
                `shouldBe`
                    Map.fromList
                        [ (("A",1), NumV 10)
                        , (("A",2), NumV 20)
                        , (("A",3), NumV 30)
                        ]
        it "returns cycle error for cells in a cycle" $ do
            let sheet = Sheet [ Cell ("A",1) (Form (Ref ("A",2)))
                              , Cell ("A",2) (Form (Ref ("A",1)))
                             ]
            evaluateSheet sheet
                `shouldBe`
                    Map.fromList
                        [ (("A",1), ErrV "cycle")
                        , (("A",2), ErrV "cycle")
                        ]
        it "evaluates formulas independently of cell order" $ do
            let sheet = Sheet [ Cell ("A",3)
                                (Form
                                    (BinOp Add
                                        (Ref ("A",1))
                                        (Ref ("A",2))))
                                , Cell ("A",1) (Lit (NumV 10))
                                , Cell ("A",2) (Lit (NumV 20))
                            ]
            evaluateSheet sheet
                `shouldBe`
                Map.fromList
                    [ (("A",1), NumV 10)
                    , (("A",2), NumV 20)
                    , (("A",3), NumV 30)
                    ]
        it "returns error for unknown cell reference" $ do
            let sheet =
                    Sheet[ Cell ("A",1)
                            (Form
                                (Ref ("B",1)))
                         ]
            evaluateSheet sheet
                `shouldBe`
                Map.fromList
                [ (("A",1), ErrV "Unknown cell")
                ]

        it "returns error for division by zero" $ do
            let sheet =
                    Sheet [ Cell ("A",1)
                            (Form
                                (BinOp Div
                                (LitE (NumV 10))
                                (LitE (NumV 0))))
                    ]
            evaluateSheet sheet
                `shouldBe`
                Map.fromList
                [ (("A",1), ErrV "Division by zero")
                ]

        it "propagates errors through formulas" $ do
            let sheet =
                    Sheet [ Cell ("A",1)
                            (Form
                                (Ref ("B",1)))
                            , Cell ("A",2)
                                (Form
                                    (BinOp Add
                                    (Ref ("A",1))
                                    (LitE (NumV 5))))
                        ]

            evaluateSheet sheet
                `shouldBe`
                Map.fromList
                [ (("A",1), ErrV "Unknown cell")
                , (("A",2), ErrV "Unknown cell")
                ]

    describe "Dependency graph" $ do
        it "extracts dependencies from formulas" $ do
            let sheet = Sheet [ Cell ("A",1) (Lit (NumV 10))
                              , Cell ("A",2) (Lit (NumV 20))
                              , Cell ("A",3)
                                (Form
                                    (BinOp Add
                                        (Ref ("A",1))
                                        (Ref ("A",2))))
                              ]
            buildDependencyGraph sheet
                `shouldBe`
                    Map.fromList
                        [ (("A",1), [])
                        , (("A",2), [])
                        , (("A",3),
                            [ ("A",1)
                            , ("A",2)
                            ])
                        ]

    describe "Cycle detection" $ do
        it "detects a simple cycle" $ do
            let sheet = Sheet [ Cell ("A",1)
                                (Form (Ref ("A",2)))
                              , Cell ("A",2)
                                (Form (Ref ("A",1)))
                              ]
            findCycles
                (buildDependencyGraph sheet)
                    `shouldBe`
                    [("A",1),("A",2)]
