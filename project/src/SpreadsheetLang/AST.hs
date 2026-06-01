module SpreadsheetLang.AST where

type Addr = (String, Int)

data Sheet = Sheet [Cell]
  deriving (Eq, Show)

data Cell = Cell
  { addr :: Addr
  , content :: Content
  }
  deriving (Eq, Show)

data Content
  = Lit Value
  | Form Expr
  deriving (Eq, Show)

data Expr
  = Ref Addr
  | LitE Value
  | BinOp Op Expr Expr
  | RangeOp RangeOp Addr Addr
  deriving (Eq, Show)

data Value
  = NumV Double
  | BoolV Bool
  | StrV String
  | ErrV String
  deriving (Eq, Show)

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Eq, Show)

data RangeOp
  = SumR
  | AvgR
  deriving (Eq, Show)