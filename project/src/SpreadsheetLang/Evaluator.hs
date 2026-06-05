module SpreadsheetLang.Evaluator where

import SpreadsheetLang.AST

import qualified Data.Map as Map

type CellMap = Map.Map Addr Value

evaluateSheet :: Sheet -> CellMap
evaluateSheet(Sheet cells) =
    Map.fromList $
        map evaluateCell cells

evaluateCell :: Cell -> (Addr, Value)
evaluateCell (Cell a content) =
    (a, evaluateContent content)

evaluateContent :: Content -> Value
evaluateContent (Lit v) = v
evaluateContent (Form _) = 
    ErrV "Formula evaluation not implemented yet"
