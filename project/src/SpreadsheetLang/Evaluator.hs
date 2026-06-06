module SpreadsheetLang.Evaluator where

import SpreadsheetLang.AST
import SpreadsheetLang.Dependency
import SpreadsheetLang.CycleDetection


import qualified Data.Map as Map
import qualified Data.Set as Set

type CellMap = Map.Map Addr Value

evaluateSheet :: Sheet -> CellMap
evaluateSheet sheet@(Sheet cells) =
    foldl evaluateOne initialEnv cells
    where
        graph = buildDependencyGraph sheet
        cycleCells = findCycles graph
        initialEnv = Map.fromList 
            [ (addr, ErrV "cycle")
            | addr <- cycleCells
            ]

evaluateOne :: CellMap -> Cell -> CellMap
evaluateOne env (Cell addr content) =
    case Map.lookup addr env of
        Just (ErrV "cycle") -> env
        _ ->  let value = evaluateContent env content
              in Map.insert addr value env

-- evaluateCell :: Cell -> (Addr, Value)
-- evaluateCell (Cell a content) =
    -- (a, evaluateContent content)

evaluateContent :: CellMap -> Content -> Value
evaluateContent _ (Lit v) =
    v
evaluateContent env (Form expr) =
    evaluateExpr env expr

evaluateExpr :: CellMap -> Expr -> Value
evaluateExpr env (LitE v) = 
    v
evaluateExpr env (Ref addr) =
    Map.findWithDefault 
        (ErrV "Reference not found") 
        addr 
        env
evaluateExpr env (BinOp op e1 e2) =
    case (evaluateExpr env e1, evaluateExpr env e2) of
        (NumV x, NumV y) -> 
            case op of
                Add -> NumV (x + y)
                Sub -> NumV (x - y)
                Mul -> NumV (x * y)
                Div -> if y == 0
                        then ErrV "Division by zero"
                        else NumV (x / y)
        _ -> ErrV "Type error in binary operation"
