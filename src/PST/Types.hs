module PST.Types (SExpr (..)) where

data SExpr
  = Symbol String
  | Node [SExpr]

instance Show SExpr where
  show (Symbol s) = s
  show (Node []) = "()"
  show (Node (x : xs)) =
    "(" ++ show x ++ concatMap (\expr -> " " ++ show expr) xs ++ ")"
