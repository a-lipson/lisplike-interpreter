{-# LANGUAGE LambdaCase #-}

module AST.Pattern where

import           AST.Error         (InterpreterError (AbstractSyntaxError))
import           AST.Types         (Pattern (..))
import           Control.Exception (throw)
import           PST.Types         (SExpr (..))
import           Text.Read         (readMaybe)

pattern :: SExpr -> Pattern
pattern p = case p of
  Symbol sym -> case readMaybe sym of
    Just i -> IntP i
    Nothing -> case sym of
      "_"         -> WildcardP
      "nil"       -> NilP
      "true"      -> BoolP True
      "false"     -> BoolP False
      '\'' : name -> SymbolP name
      _           -> throw $ AbstractSyntaxError "Invalid empty symbol pattern"
  Node [] -> throw $ AbstractSyntaxError "Expected pattern but got '()'"
  Node (h : args) -> case (h, args) of
    (Symbol "cons", [p1, p2]) -> ConsP (pattern p1) (pattern p2)
    (Symbol s, ps) -> StructP s (map pattern ps)
    _ -> throw $ AbstractSyntaxError $ "Expected pattern but got " ++ show p

vars :: Pattern -> [String]
vars = \case
  VarP s -> [s]
  ConsP p1 p2 -> vars p1 ++ vars p2
  StructP s ps -> s : concatMap vars ps
  _ -> []
