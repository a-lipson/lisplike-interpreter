module AST.Types where

newtype Env = Env [(String, Expr)] -- Exprs should be values
  deriving (Show)

data Expr
  = IntLit Int
  | BoolLit Bool
  | Nil
  | Var String
  | Sym String
  | Cons Expr Expr -- might want to change Cons to Pair, and Car/Cdr to First/Second
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Ord Expr Expr
  | Eq Expr Expr
  | If Expr Expr Expr
  | Let [(String, Expr)] Expr
  | IsNil Expr
  | IsCons Expr
  | Car Expr
  | Cdr Expr
  | Cond [(Expr, Expr)] -- Expr -- [(predicate, result)] default
  | Match Expr [(Pattern, Expr)] -- scrutinee [(pattern, result)]
  | Print Expr
  | Call Expr [Expr]
  | Lambda (Maybe String) [String] Expr -- recursiveName parameterNames body
  | Closure (Maybe String) [String] Expr Env
  | StructConstructor String [Expr] -- these do not parse
  | StructAccess String Int Expr
  | StructPredicate String Expr
  deriving (Show)

data Binding
  = VarB String Expr
  | Top Expr
  | TestB Expr
  | FuncB String [String] Expr -- functionName parameterNames body
  | StructB String [String] -- structName fieldNames

data Pattern
  = WildcardP
  | NilP
  | VarP String
  | IntP Int
  | BoolP Bool
  | SymbolP String
  | ConsP Pattern Pattern
  | StructP String [Pattern]
  deriving (Show)
