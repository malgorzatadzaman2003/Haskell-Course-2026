module SpreadsheetLang.CycleDetection where

import SpreadsheetLang.AST
import SpreadsheetLang.Dependency

import qualified Data.Map as Map

dfs 
    :: DependencyGraph
     -> [Addr] 
     -> Addr 
     -> Bool

dfs graph visited current  
    | current `elem` visited = 
        True
    | otherwise = 
        any 
            (dfs graph (current:visited)) 
            neighbors
  where
    neighbors = 
        Map.findWithDefault 
            [] 
            current 
            graph    

isInCycle 
    :: DependencyGraph 
    -> Addr 
    -> Bool

isInCycle graph addr =
    dfs graph [] addr

findCycles
    :: DependencyGraph
    -> [Addr]

findCycles graph =
    filter
        (isInCycle graph)
        (Map.keys graph)
