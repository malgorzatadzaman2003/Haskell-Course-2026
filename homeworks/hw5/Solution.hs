import Control.Monad.State
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Data.Map (Map)

-- Exercise 1: Stack machine

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
    deriving (Show)

execInstr :: Instr -> State [Int] ()
execInstr (PUSH n) = modify (n :)

execInstr POP = do
    stack <- get
    case stack of
        (_:xs) -> put xs
        []     -> return ()

execInstr DUP = do
    stack <- get
    case stack of
        (x:xs) -> put (x:x:xs)
        []     -> return ()

execInstr SWAP = do
    stack <- get
    case stack of
        (x:y:xs) -> put (y:x:xs)
        _        -> return ()

execInstr ADD = do
    stack <- get
    case stack of
        (x:y:xs) -> put ((x + y) : xs)
        _        -> return ()

execInstr MUL = do
    stack <- get
    case stack of
        (x:y:xs) -> put ((x * y) : xs)
        _        -> return ()

execInstr NEG = do
    stack <- get
    case stack of
        (x:xs) -> put ((-x) : xs)
        []     -> return ()

execProg :: [Instr] -> State [Int] ()
execProg [] = return ()
execProg (i:is) = do
    execInstr i
    execProg is

runProg :: [Instr] -> [Int]
runProg program = execState (execProg program) []

-- Exercise 2: Expression evaluator with variable bindings

data Expr
    = Num Int
    | Var String
    | Add Expr Expr
    | Mul Expr Expr
    | Neg Expr
    | Assign String Expr
    | Seq Expr Expr
    deriving (Show)

eval :: Expr -> State (Map String Int) Int
eval (Num n) = return n

eval (Var name) = do
    env <- get
    return (env Map.! name)

eval (Add e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    return (v1 + v2)

eval (Mul e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    return (v1 * v2)

eval (Neg e) = do
    v <- eval e
    return (-v)

eval (Assign name e) = do
    v <- eval e
    modify (Map.insert name v)
    return v

eval (Seq e1 e2) = do
    eval e1
    eval e2

runEval :: Expr -> Int
runEval e = evalState (eval e) Map.empty