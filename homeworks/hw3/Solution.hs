import qualified Data.Map as Map
import Data.Map (Map)

import Control.Monad (guard)
import Control.Monad.RWS (MonadWriter(tell))

import Control.Monad.Writer
-- MAYBE MONAD

-- Exercise 1: Maze navigation

type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

-- (a) move

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
    exits <- Map.lookup pos maze
    Map.lookup dir exits

-- (b) followPath

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath _ pos [] = Just pos
followPath maze pos (d:ds) = do
    next <- move maze pos d
    followPath maze next ds

-- (c) safePath

safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath _ pos [] = Just [pos]
safePath maze pos (d:ds) = do
    next <- move maze pos d
    rest <- safePath maze next ds   
    return (pos : rest)

-- Exercise 2: Decoding a message

type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (\c -> Map.lookup c key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)

-- LIST MONAD

-- Exercise 3: Seating arrangements

type Guest = String
type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
    perm <- permutationsM guests
    guard (validSeating perm)
    return perm
  where
    conflictsWith a b =
        (a, b) `elem` conflicts || (b, a) `elem` conflicts

    adjacentPairs [] = []
    adjacentPairs [_] = []
    adjacentPairs xs = zip xs (tail xs) ++ [(last xs, head xs)]

    validSeating xs = all (\(a,b) -> not (conflictsWith a b)) (adjacentPairs xs)

permutationsM :: Eq a => [a] -> [[a]]
permutationsM [] = return []
permutationsM xs = do
    x <- xs
    rest <- permutationsM (removeFirst x xs)
    return (x : rest)

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst y (x:xs)
    | y == x    = xs
    | otherwise = x : removeFirst y xs

-- CUSTOM MONAD

-- Exercise 4: Result monad with warnings

data Result a = Failure String | Success a [String]
    deriving Show

-- (a) Functor, Applicative, Monad instances

instance Functor Result where
    fmap _ (Failure msg) = Failure msg
    fmap f (Success x warns) = Success (f x) warns

instance Applicative Result where
    pure x = Success x []
    Failure msg <*> _ = Failure msg
    Success _ warns <*> Failure msg = Failure msg
    Success f warns1 <*> Success x warns2 = Success (f x) (warns1 ++ warns2)

instance Monad Result where
    Failure msg >>= _ = Failure msg
    Success x warns1 >>= f =
        case f x of
            Failure msg -> Failure msg
            Success y warns2 -> Success y (warns1 ++ warns2)
            
-- (b) Write helper functions: warn, failure

warn :: String -> Result ()
warn msg = Success () [msg] 

failure :: String -> Result a
failure = Failure    

-- (c) Use the Result monad to implement a functions: validateAge, validateAges

validateAge :: Int -> Result Int
validateAge n
    | n < 0     = failure "FAILURE!: Negative age"
    | n > 150   = do
        warn "WARNING!: Age above 150"
        return n
    | otherwise = return n

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

-- WRITER MONAD

-- Exercise 5: Evaluator with simplification log

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr
    deriving Show

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)

simplify (Neg e) = do
    e' <- simplify e
    case e' of
        Neg inner -> do
            tell ["Double negation: -(-e) -> e"]
            return inner
        _ -> return (Neg e')