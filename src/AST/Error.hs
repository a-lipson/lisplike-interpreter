module AST.Error where

import           Control.Exception
import           PST.Types         (SExpr)

data InterpreterError
  = AbstractSyntaxError String
  | RuntimeError String
  deriving (Show)

instance Exception InterpreterError

sexprError :: String -> SExpr -> InterpreterError
sexprError info sexpr = AbstractSyntaxError $ info ++ " " ++ show sexpr
