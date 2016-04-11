module AbstractSyntax where

type Value = Bool
type Output = [Value]

data Var = X | Y deriving Eq

instance Show Var where
  show X = "x"
  show Y = "y"

data Stmt =
    Print Exp Stmt
  | Assign Var Exp Stmt
  | End
  deriving (Eq, Show)

data Exp =
    Variable Var
  | Value Bool
  | And Exp Exp
  | Or Exp Exp
  deriving (Eq, Show)

data Type =
    TyBool
  | TyVoid
  deriving (Eq, Show)

fold :: (Var -> b) -> (Bool -> b) -> (b -> b -> b) -> (b -> b -> b) -> Exp -> b
fold v bo a o (Variable x) = v x
fold b bo a o (Value x) = bo x
fold v bo a o (And e1 e2) = a (fold v bo a o e1) (fold v bo a o e2)
fold v bo a o (Or e1 e2) = o (fold v bo a o e1) (fold v bo a o e2)

size :: Exp -> Integer
size = fold (\_ -> 1) (\_-> 1) (\x y -> x+y+1) (\x y -> x+y+1)

foldStmt :: (Exp -> b -> b) -> (Var -> Exp -> b -> b) -> b ->  Stmt -> b
foldStmt e ve ed (Print exp s) = e exp (foldStmt e ve ed s)
foldStmt e ve ed (Assign v exp s) =ve v exp (foldStmt e ve ed s)
foldStmt e ve ed End = ed

--eof
