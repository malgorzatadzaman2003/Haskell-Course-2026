module Main where

import SpreadsheetLang.Parser
import Text.Megaparsec

main :: IO ()
main = do
    let input =
            "sheet {\n\
            \  A1 = 10;\n\
            \  A2 = 20;\n\
            \}"

    print $
        parse parseSheet "" input