{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module AST.Expr where

import           AST.Error   (InterpreterError (..), sexprError)
import           AST.Pattern (pattern)
import           AST.Types   (Expr (..))
import           PST.Types   (SExpr (..))
import           Text.Read   (readMaybe)

expr :: SExpr -> Either InterpreterError Expr
expr (Symbol sym) = parseSymbol sym
expr (Node []) = Left $ AbstractSyntaxError "Expected expression but got '()'"
expr n@(Node (h : args)) = parseNode h args n

parseSymbol :: String -> Either InterpreterError Expr
parseSymbol = \case
  ['\''] -> Left $ AbstractSyntaxError "Invalid empty symbol"
  '\'' : name -> pure $ Sym name
  sym -> case readMaybe sym of
    Just i -> pure $ IntLit i
    _ -> case sym of
      "true"  -> pure $ BoolLit True
      "false" -> pure $ BoolLit False
      "nil"   -> pure Nil
      _       -> pure $ Var sym

parseNode :: SExpr -> [SExpr] -> SExpr -> Either InterpreterError Expr
parseNode (Symbol name) args original = case name of
  "print"  -> op "print" Print args
  "car"    -> op "car" Car args
  "cdr"    -> op "cdr" Cdr args
  "cons?"  -> op "cons?" IsCons args
  "nil?"   -> op "nil?" IsNil args
  "+"      -> binop "+" Add args
  "-"      -> binop "-" Sub args
  "*"      -> binop "*" Mul args
  "/"      -> binop "/" Div args
  "cons"   -> binop "cons" Cons args
  "if"     -> parseIf args original
  "let"    -> parseLet args original
  "cond"   -> parseCond args
  "match"  -> parseMatch args original
  "lambda" -> parseLambda args original
  _        -> Call <$> expr (Symbol name) <*> traverse expr args
parseNode f args _ = Call <$> expr f <*> traverse expr args

parseIf :: [SExpr] -> SExpr -> Either InterpreterError Expr
parseIf [branch, thn, els] _ = If <$> expr branch <*> expr thn <*> expr els
parseIf _ _                  = Left $ expectArgumentsError 3 "if expression"

parseLet :: [SExpr] -> SExpr -> Either InterpreterError Expr
parseLet [Node defs, body] _ = Let [] <$> expr body
parseLet _ _                 = Left $ expectArgumentsError 2 "let expression"

parseCond :: [SExpr] -> Either InterpreterError Expr
parseCond clauses = Cond <$> traverse pair clauses
 where
  pair (Node [pred, body]) = (,) <$> expr pred <*> expr body
  pair p = Left $ sexprError "Malformed clause in cond expression" p

parseMatch :: [SExpr] -> SExpr -> Either InterpreterError Expr
parseMatch (h : clauses) _ = Match <$> expr h <*> traverse pair clauses
 where
  pair (Node [pat, body]) = (pattern pat,) <$> expr body
  pair p = Left $ sexprError "Malformed clause in match expression" p
parseMatch _ orig = Left $ sexprError "Match expression missing scrutinee expression" orig

parseLambda :: [SExpr] -> SExpr -> Either InterpreterError Expr
parseLambda [Node params, body] _ = Lambda Nothing [] <$> expr body
parseLambda [Node _] orig = Left $ sexprError "Lambda expression missing body" orig
parseLambda _ orig = Left $ sexprError "Lambda expression missing arguments" orig

op sym c = \case
  [p] -> c <$> expr p
  _ -> Left $ expectArgumentsError 1 ("operator " ++ sym)

binop sym c = \case
  [l, r] -> c <$> expr l <*> expr r
  _ -> Left $ expectArgumentsError 2 ("operator " ++ sym)

expectArgumentsError :: Int -> String -> InterpreterError
expectArgumentsError n info = AbstractSyntaxError $ info ++ " expects " ++ show n ++ " args but got " ++ show p

{-
expr :: SExpr -> Either InterpreterError Expr
expr p = case p of
  Symbol sym -> case sym of
    ['\''] -> Left $ AbstractSyntaxError "Invalid empty symbol"
    '\'' : name -> pure $ Sym name
    _ -> case readMaybe sym of
      Just i -> pure $ IntLit i
      _ -> case sym of
        "true"  -> pure $ BoolLit True
        "false" -> pure $ BoolLit False
        "nil"   -> pure Nil
        _       -> pure $ Var sym
  Node [] -> Left $ AbstractSyntaxError "Expected expression but got '()'"
  Node (h : args) -> case (h, args) of
    (Symbol "print", args) -> op "print" Print args
    (Symbol "car", args) -> op "car" Car args
    (Symbol "cdr", args) -> op "cdr" Cdr args
    (Symbol "cons?", args) -> op "cons?" IsCons args
    (Symbol "nil?", args) -> op "nil?" IsNil args
    (Symbol "+", args) -> binop "+" Add args
    (Symbol "-", args) -> binop "-" Sub args
    (Symbol "*", args) -> binop "*" Mul args
    (Symbol "/", args) -> binop "/" Div args
    (Symbol "cons", args) -> binop "cons" Cons args
    (Symbol "if", [branch, thn, els]) -> If <$> expr branch <*> expr thn <*> expr els
    (Symbol "if", _) -> Left $ expectArgumentsError 3 "if expression"
    (Symbol "let", [Node defs, body]) -> Let [] <$> expr body
    (Symbol "let", _) -> Left $ expectArgumentsError 2 "let expression"
    (Symbol "cond", clauses) -> Cond <$> traverse pair clauses
     where
      pair = \case
        Node [pred, body] -> (,) <$> expr pred <*> expr body
        p -> Left $ sexprError "Malformed clause in cond expression" p
    (Symbol "match", h : clauses) -> Match <$> expr h <*> traverse pair clauses
     where
      pair = \case
        Node [pat, body] -> (pattern pat,) <$> expr body
        p -> Left $ sexprError "Malformed clause in match expression" p
    (Symbol "match", _) -> Left $ sexprError "Match expression missing scrutinee expression" p
    (Symbol "lambda", [Node params, body]) -> Lambda Nothing [] <$> expr body
    (Symbol "lambda", [Node _]) -> Left $ sexprError "Lambda expression missing body" p
    (Symbol "lambda", _) -> Left $ sexprError "Lambda expression missing arguments" p
    (f, args) -> Call <$> expr f <*> traverse expr args
 where
  op sym c = \case
    [p] -> c <$> expr p
    _ -> Left $ expectArgumentsError 1 ("operator " ++ sym)
  binop sym c = \case
    [l, r] -> c <$> expr l <*> expr r
    _ -> Left $ expectArgumentsError 2 ("operator " ++ sym)

  expectArgumentsError :: Int -> String -> InterpreterError
  expectArgumentsError n info = AbstractSyntaxError $ info ++ " expects " ++ show n ++ " args but got " ++ show p
-}
