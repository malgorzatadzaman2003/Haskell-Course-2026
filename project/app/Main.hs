module Main where

import SpreadsheetLang.Parser
import Text.Megaparsec

main :: IO ()
main = do

    print $
        parse parseCell "" "A1 = 10;"