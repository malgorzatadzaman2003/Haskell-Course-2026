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

movePlayer :: Int -> AdventureGame Int
movePlayer dice = do
    st <- get
    let currentPos = position st
        currentEnergy = energy st
        moved = min dice currentEnergy
        newPos = min 8 (currentPos + moved)
        newEnergy = currentEnergy - moved
    put st { position = newPos, energy = newEnergy }
    return moved

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
    choice <- lift (getPlayerChoice options)
    modify (\st -> st { pathName = choice })
    return choice

-- 5: Game loop

handleLocation :: AdventureGame Bool
handleLocation = do
    st <- get
    let loc = Map.findWithDefault Normal (position st) board
    case loc of
        Normal -> do
            lift (putStrLn "You are on a quiet path.")
            return False

        Decision options -> do
            lift (putStrLn "You reached a decision point.")
            choice <- makeDecision options
            lift (putStrLn ("You chose: " ++ choice))
            return False

        Obstacle cost -> do
            lift (putStrLn ("Obstacle! You lose " ++ show cost ++ " extra energy."))
            modify (\s -> s { energy = max 0 (energy s - cost) })
            return False

        Treasure points -> do
            lift (putStrLn ("Treasure found! +" ++ show points ++ " points."))
            modify (\s -> s { score = score s + points })
            return False

        Trap points -> do
            lift (putStrLn ("Trap! You lose " ++ show points ++ " points."))
            modify (\s -> s { score = max 0 (score s - points) })
            return False

        Goal -> do
            lift (putStrLn "You reached the main treasure!")
            return True

playTurn :: AdventureGame Bool
playTurn = do
    st <- get
    if energy st <= 0
        then do
            lift (putStrLn "You ran out of energy!")
            return True
        else do
            dice <- lift getDiceRoll
            moved <- movePlayer dice
            lift (putStrLn ("You moved " ++ show moved ++ " spaces."))
            ended <- handleLocation
            newState <- get
            lift (displayGameState newState)
            if energy newState <= 0
                then do
                    lift (putStrLn "Game over: no energy left.")
                    return True
                else return ended

playGame :: AdventureGame ()
playGame = do
    ended <- playTurn
    if ended
        then lift (putStrLn "Thanks for playing Treasure Hunters!")
        else playGame

-- 5: User interaction in IO
getDiceRoll :: IO Int
getDiceRoll = do
    putStrLn "Enter dice roll (1-6):"
    input <- getLine
    case reads input of
        [(n, "")] | n >= 1 && n <= 6 -> return n
        _ -> do
            putStrLn "Invalid dice roll. Please enter a number from 1 to 6."
            getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState st = do
    putStrLn "----- Game State -----"
    putStrLn ("Position: " ++ show (position st))
    putStrLn ("Energy:   " ++ show (energy st))
    putStrLn ("Score:    " ++ show (score st))
    putStrLn ("Path:     " ++ pathName st)
    putStrLn "----------------------"

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
    putStrLn "Choose a path:"
    mapM_ putStrLn options
    input <- getLine
    if input `elem` options
        then return input
        else do
            putStrLn "Invalid choice. Try again."
            getPlayerChoice options

initialGameState :: GameState
initialGameState = GameState
    { position = 0
    , energy = 15
    , score = 0
    , pathName = "start"
    }

main :: IO ()
main = do
    putStrLn "Welcome to Treasure Hunters!"
    displayGameState initialGameState
    evalStateT playGame initialGameState