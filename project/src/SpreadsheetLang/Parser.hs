module SpreadsheetLang.Parser where

import SpreadsheetLang.AST

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

-- Parser for cell addresses, e.g., "A1", "B25", etc.
parseAddr :: Parser Addr
parseAddr = do
    col <- some upperChar
    row <- some digitChar

    pure (col, read row)

-- Parser for numeric literals, e.g., "10", "3.14", etc.
parseNumber :: Parser Value
parseNumber = do
    n <- some digitChar
    pure (NumV (read n))

-- Parser for a cell, e.g., "A1 = 10;"
parseCell:: Parser Cell
parseCell = do
    addr <- parseAddr
    space
    char '='
    space
    val <- parseNumber
    char ';'
    pure $
        Cell  addr  (Lit val)


