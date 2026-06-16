module SpreadsheetLang.Dependency where

import SpreadsheetLang.AST

import qualified Data.Map as Map

type DependencyGraph =
    Map.Map Addr [Addr]

exprDependencies :: Expr -> [Addr]
exprDependencies (Ref addr) =
    [addr]
exprDependencies (LitE _) =
    []
exprDependencies (BinOp _ e1 e2) =
    exprDependencies e1 ++
    exprDependencies e2
exprDependencies (RangeOp _ a1 a2) =
    expandRange a1 a2

expandRange :: Addr -> Addr -> [Addr]
expandRange (startCol, startRow) (endCol, endRow)
    | startCol == endCol =
        [ (startCol, row)
        | row <- [startRow .. endRow]
        ]
    | otherwise =
        []

cellDependencies :: Cell -> [Addr]
cellDependencies (Cell _ (Lit _)) =
    []
cellDependencies (Cell _ (Form expr)) =
    exprDependencies expr

buildDependencyGraph :: Sheet -> DependencyGraph
buildDependencyGraph (Sheet cells) =
    Map.fromList $
        map makeEntry cells
  where
    makeEntry cell@(Cell addr _) =
        (addr, cellDependencies cell)