module PST.Token where

data Token
  = SymbolT String
  | OpenParen
  | CloseParen
  | EOFT
  deriving (Show)
