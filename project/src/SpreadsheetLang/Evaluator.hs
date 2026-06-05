module SpreadsheetLang.Evaluator where

import SpreadsheetLang.AST

import qualified Data.Map as Map

type CellMap = Map.Map Addr Value

evaluateSheet :: Sheet -> CellMap
evaluateSheet (Sheet cells) =
    foldl evaluateOne Map.empty cells

evaluateOne :: CellMap -> Cell -> CellMap
evaluateOne env (Cell addr content) =
    let value =
            evaluateContent env content
    in
        Map.insert addr value env

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
