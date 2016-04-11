module TypeCheck where

import Error
import AbstractSyntax

type TypeEnvironment = [(Var, Type)]

typeVar :: Var -> TypeEnvironment -> ErrorOr Type
typeVar x' ((x,t):xvs) = if x == x' then Result t else typeVar x' xvs
typeVar x'  _          = TypeError (show x' ++ " is not bound.")

class Typeable a where
  check :: TypeEnvironment -> a -> ErrorOr Type

instance Typeable Stmt where
  check env End = Result TyVoid
  check env (Print e s) = if (check env s) == Result TyVoid && (check env e) == Result TyBool then Result TyVoid else TypeError "Invalid Print statement."
  check env (Assign x e s) = let t = (check env e) in
                                 case t of
                                   Result tt -> if (check (env ++[(x,tt)]) s) == Result TyVoid then Result TyVoid else TypeError "Invalid Assign statement."
                                   TypeError _ -> TypeError "Invalid Assign statement."
  check _   _      = TypeError "Invalid statement."

instance Typeable Exp where
  check env (Value True) = Result TyBool
  check env (Value False) = Result TyBool
  check env (Variable X) = typeVar X env
  check env (Variable Y) = typeVar Y env
  check env (And e1 e2) = if (check env e1) == Result TyBool && (check env e2) == Result TyBool then Result TyBool else TypeError "Invalid And expression."
  check env (Or e1 e2) = if (check env e1) == Result TyBool && (check env e2) == Result TyBool then Result TyBool else TypeError "Invalid Or expression."
  check _   _      = TypeError "Invalid expression."

typeCheck :: Typeable a => a -> ErrorOr (a, Type)
typeCheck ast = liftErr (\t -> (ast, t)) (check [] ast) -- Pair result with its type.

--eof
