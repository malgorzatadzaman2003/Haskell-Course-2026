module Main where

import SpreadsheetLang.Parser
import SpreadsheetLang.Evaluator

import System.Environment (getArgs)
import Text.Megaparsec

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            input <- readFile filePath
            case parse parseSheet filePath input of
                Left err -> putStrLn (errorBundlePretty err)
                Right sheet -> print (evaluateSheet sheet)
        _ -> putStrLn "Usage: spreadsheet-lang <file.sheet>"         