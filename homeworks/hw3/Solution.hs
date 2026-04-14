import qualified Data.Map as Map
import Data.Map (Map)

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
