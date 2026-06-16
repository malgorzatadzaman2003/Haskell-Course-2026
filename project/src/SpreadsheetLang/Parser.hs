module SpreadsheetLang.Parser where

import SpreadsheetLang.AST

import Data.Void
import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad.Combinators.Expr

import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- add support for comments and spaceConsumer
lineComment :: Parser ()
lineComment = do
    _ <- string "--"
    _ <- manyTill anySingle (void eol <|> eof)
    pure ()

spaceConsumer :: Parser ()
spaceConsumer =
    L.space space1 lineComment empty

-- after symbols such as +, -, *, /, we consume any trailing whitespace
symbol :: String -> Parser String
symbol s = do
    result <- string s
    spaceConsumer
    pure result


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

-- Parser for expressions, e.g., "A1 + 10", "B2 * C3", etc.
parseNumberExpr :: Parser Expr
parseNumberExpr = do
    val <- parseNumber
    spaceConsumer
    pure (LitE val)

-- Parser for cell references, e.g., "A1", "B2", etc.
parseRefExpr :: Parser Expr
parseRefExpr = do
    addr <- parseAddr
    spaceConsumer
    pure (Ref addr)

-- Parser for terms, which can be either a number, a cell reference, or a parenthesized expression
parseTerm :: Parser Expr
parseTerm =
        parseRangeExpr
    <|> parseNumberExpr
    <|> parseRefExpr
    <|> between (symbol "(") (symbol ")") parseExpr

-- Helper function to create binary operator parsers
binary :: String -> Op -> Operator Parser Expr
binary name op =
    InfixL $ do
        _ <- symbol name
        pure (BinOp op)

-- Operator priority: * and / bind tighter than + and -
parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm operators
  where
    operators =
        [ [ binary "*" Mul
          , binary "/" Div
          ]
        , [ binary "+" Add  
          , binary "-" Sub
          ]
        ]   

exprToContent :: Expr -> Content
exprToContent (LitE val) = Lit val      
exprToContent expr = Form expr

-- Parser for a cell, e.g., "A1 = 10;"
parseCell:: Parser Cell
parseCell = do
    addr <- parseAddr
    spaceConsumer
    char '='
    spaceConsumer
    expr <- parseExpr
    char ';'
    spaceConsumer   
    pure $
        Cell  addr  (exprToContent expr)

-- Parser for a sheet, e.g.,
-- sheet {
--   A1 = 10;
--   A2 = 20;
-- }        
parseSheet :: Parser Sheet  
parseSheet = do
    spaceConsumer
    string "sheet"
    spaceConsumer
    char '{'
    spaceConsumer   
    cells <- many parseCell
    spaceConsumer
    char '}'
    spaceConsumer
    eof
    pure (Sheet cells)

parseRangeExpr :: Parser Expr
parseRangeExpr =
        parseSumRange
    <|> parseAvgRange

parseSumRange :: Parser Expr
parseSumRange = do
    _ <- string "SUM"
    spaceConsumer
    _ <- char '('
    spaceConsumer
    start <- parseAddr
    spaceConsumer
    _ <- char ':'
    spaceConsumer
    end <- parseAddr
    spaceConsumer
    _ <- char ')'
    spaceConsumer
    pure (RangeOp SumR start end)

parseAvgRange :: Parser Expr
parseAvgRange = do
    _ <- string "AVG"
    spaceConsumer
    _ <- char '('
    spaceConsumer
    start <- parseAddr
    spaceConsumer
    _ <- char ':'
    spaceConsumer
    end <- parseAddr
    spaceConsumer
    _ <- char ')'
    spaceConsumer
    pure (RangeOp AvgR start end)