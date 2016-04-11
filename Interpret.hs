module Interpret where

import Error
import AbstractSyntax
import TypeCheck

type ValueEnvironment = [(Var, Value)]

valueVar :: Var -> ValueEnvironment -> Value
valueVar x' ((x,v):xvs) = if x == x' then v else valueVar x' xvs

evaluate :: ValueEnvironment -> Exp -> Value
evaluate env e = fold (\v -> valueVar v env) (\b -> b) (\x y -> x && y) (\x y -> x || y) e

execute :: ValueEnvironment -> Stmt -> (ValueEnvironment, Output)
execute env End = ([],[])
execute env (Print e s) = let (v,o) = execute env s in
                            (v, [(evaluate env e)]++o)
execute env (Assign x e s) = let v = evaluate env e in
                               execute ([(x,v)] ++ env) s  -- put (x,v) in front, this will overwrite old value
                                 
interpret :: Stmt -> ErrorOr Output
interpret s = liftErr (\(x,o) -> snd (execute [] x)) (typeCheck s)


-- interpret (Assign X (And (And (Value True) (Value True)) (And (Value True) (Value True))) (Assign X (And (Variable X) (Value False)) (Print (Variable X) End)))
-- should return Result [False]
--eof
