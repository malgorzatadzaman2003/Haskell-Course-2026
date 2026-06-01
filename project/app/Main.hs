module Main where

import SpreadsheetLang.Parser
import Text.Megaparsec

main :: IO ()
main = do
    let input =
            "sheet {\n\
            \  A1 = 10;\n\
            \  A2 = 20;\n\
            \  A3 = A1 + A2;\n\
            \  A4 = A3 * 2;\n\
            \}"

    print $
        parse parseSheet "" input