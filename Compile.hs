module Compile where

import Error
import AbstractSyntax
import TypeCheck
import Machine

convert :: Value -> Integer
convert True = 1
convert False = 0

converts :: Output -> Buffer
converts = map convert

type AddressEnvironment = [(Var, Address)]

addressVar :: Var -> AddressEnvironment -> Address
addressVar x' ((x,a):xas) = if x == x' then a else addressVar x xas

class Compilable a where
  compile :: AddressEnvironment -> a -> [Instruction]

-- X in mem location 7
-- Y in mem location 8
-- And  x mul y
-- Or   x add y - (x mul y)

hp :: Exp -> Integer
hp e = (size e) + 8

copy :: Integer->Integer->[Instruction]
copy a b = [SET 3 a,SET 4 b,COPY]

-- add content of addr a and addr b, result is in mem 0
add :: Integer->Integer->[Instruction]
add a b = (copy a 1)++(copy b 2)++[ADD]

mul :: Integer->Integer->[Instruction]
mul a b = (copy a 1)++(copy b 2)++[MUL]



instance Compilable Exp where
  compile env (Variable X) = copy 7 9 --put to heap
  compile env (Variable Y) = copy 8 9
  compile env (Value True) = [SET 9 1]
  compile env (Value False) = [SET 9 0]
  compile env (And e1 e2) = (compile env e1) ++ (copy (hp e1) (hp e2 + 1)) ++ (compile env e2) ++ (mul  (hp e2 +1)  (hp e2)) ++ (copy 0 (hp (And e1 e2)))
  compile env (Or e1 e2) =  (compile env e1) ++ (copy (hp e1) (hp e2 + 1))  ++ (compile env e2) ++ (add (hp e2 +1) (hp e2)) ++(copy 0 (hp e2))++[MUL]++(copy 0 1)++[SET 2 (-1),MUL]++(add 0 (hp e2))++(copy 0 (hp (Or e1 e2)))

instance Compilable Stmt where
  compile env (Assign X exp stmt) = (compile env exp) ++(copy (hp exp) 7) ++ (compile env stmt)
  compile env (Assign Y exp stmt) = (compile env exp) ++(copy (hp exp) 8) ++ (compile env stmt)
  compile env (Print exp stmt) = (compile env exp) ++ (copy (hp exp) 5) ++ (compile env stmt)
  compile env (End) = []

compileSimulate  :: Stmt -> ErrorOr Buffer
compileSimulate s = case (check [] s) of 
                      Result _ ->  Result (simulate (compile [] s))
                      TypeError _ -> TypeError "type checking failed"

-- compileSimulate (Print (And (Value True) (Value False)) $ Print (Value True) $ End)
-- compileSimulate (Print (And (Value True) (Value True)) $ Print (Or (Value True) (Value False)) $ End)
-- compileSimulate (Assign X (Value True) $ Assign Y (Value False) $ Print (And (Variable X) (Variable Y)) $ End)
-- compileSimulate (Assign X (Value True) $ Assign Y (Value False) $ Print (Or (Variable X) (Variable Y)) $ End)
--eof
