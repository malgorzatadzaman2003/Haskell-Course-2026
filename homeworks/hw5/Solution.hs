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

-- Exercise 3: Memoised edit (Levenshtein) distance

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
    cache <- get
    case Map.lookup (i, j) cache of
        Just value -> return value
        Nothing -> do
            value <- compute
            modify (Map.insert (i, j) value)
            return value
  where
    compute
        | i == 0 = return j
        | j == 0 = return i
        | xs !! (i - 1) == ys !! (j - 1) =
            editDistM xs ys (i - 1) (j - 1)
        | otherwise = do
            deletion <- editDistM xs ys (i - 1) j
            insertion <- editDistM xs ys i (j - 1)
            substitution <- editDistM xs ys (i - 1) (j - 1)
            return (1 + minimum [deletion, insertion, substitution])

editDistance :: String -> String -> Int
editDistance xs ys =
    evalState (editDistM xs ys (length xs) (length ys)) Map.empty

-- StateT and "Treasure Hunters" Game Simulation

-- game data types + board 
data Location
    = Normal
    | Decision [String]
    | Obstacle Int
    | Treasure Int
    | Trap Int
    | Goal
    deriving (Show)

data GameState = GameState
    { position :: Int
    , energy   :: Int
    , score    :: Int
    , pathName :: String
    } deriving (Show)

type AdventureGame a = StateT GameState IO a

board :: Map Int Location
board = Map.fromList
    [ (0, Normal)
    , (1, Treasure 10)
    , (2, Decision ["forest", "cave"])
    , (3, Obstacle 2)
    , (4, Treasure 20)
    , (5, Trap 15)
    , (6, Obstacle 3)
    , (7, Treasure 30)
    , (8, Goal)
    ]

-- 4: Player movement and decisions

