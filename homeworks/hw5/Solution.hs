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