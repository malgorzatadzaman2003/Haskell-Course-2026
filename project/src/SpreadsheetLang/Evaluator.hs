module SpreadsheetLang.Evaluator where

import SpreadsheetLang.AST
import SpreadsheetLang.Dependency
import SpreadsheetLang.CycleDetection


import qualified Data.Map as Map
import qualified Data.Set as Set

type CellMap = Map.Map Addr Value
type CellDefs = Map.Map Addr Content

evaluateSheet :: Sheet -> CellMap
evaluateSheet sheet@(Sheet cells) =
    foldl evaluateAddress initialEnv allAddresses
    where
        definitions :: CellDefs
        definitions = Map.fromList
                [ (addr, content)
                | Cell addr content <- cells
                ]
        graph = buildDependencyGraph sheet
        cycleCells = findCycles graph
        initialEnv = Map.fromList 
            [ (addr, ErrV "cycle")
            | addr <- cycleCells
            ]
        allAddresses = Map.keys definitions
        evaluateAddress env addr = fst (evalCell definitions env addr)

-- evaluateOne :: CellMap -> Cell -> CellMap
-- evaluateOne env (Cell addr content) =
--     case Map.lookup addr env of
--         Just (ErrV "cycle") -> env
--         _ ->  let value = evaluateContent env content
--               in Map.insert addr value env

evalCell :: CellDefs -> CellMap -> Addr -> (CellMap, Value)
evalCell definitions env addr =
    case Map.lookup addr env of
        Just value -> (env, value)
        Nothing ->
            case Map.lookup addr definitions of
                Nothing ->
                    (env, ErrV "Unknown cell")
                Just content ->
                    let (env', value) =
                            evaluateContent definitions env content
                    in
                        (Map.insert addr value env', value)

evaluateContent :: CellDefs -> CellMap -> Content -> (CellMap, Value)
evaluateContent _ env (Lit v) =
    (env, v)

evaluateContent definitions env (Form expr) =
    evalExpr definitions env expr

evalExpr :: CellDefs -> CellMap -> Expr -> (CellMap, Value)
evalExpr _ env (LitE v) =
    (env, v)

evalExpr definitions env (Ref addr) =
    evalCell definitions env addr

evalExpr definitions env (BinOp op e1 e2) =
    let (env1, v1) =
            evalExpr definitions env e1
        (env2, v2) =
            evalExpr definitions env1 e2
    in
        case (v1, v2) of
            (NumV x, NumV y) ->
                ( env2
                , case op of
                    Add -> NumV (x + y)
                    Sub -> NumV (x - y)
                    Mul -> NumV (x * y)
                    Div ->
                        if y == 0
                        then ErrV "Division by zero"
                        else NumV (x / y)
                )
            (ErrV err, _) ->
                (env2, ErrV err)
            (_, ErrV err) ->
                (env2, ErrV err)
            _ ->
                (env2, ErrV "Type error")

evalExpr _ env (RangeOp _ _ _) =
    (env, ErrV "Range operations not implemented yet")