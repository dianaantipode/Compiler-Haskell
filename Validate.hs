module Exhaustive where

import Error
import AbstractSyntax
import Parse
import TypeCheck
import Interpret
import Compile
import Machine

type Height = Integer
type Quantity = Integer

class Exhaustive a where
  exhaustive :: Integer -> [a]

instance Exhaustive Stmt where
  exhaustive 0 = []
  exhaustive 1 = [End]
  exhaustive n = let el = (exhaustive (n-1))::[Exp] in
                   let sl = (exhaustive (n-1))::[Stmt] in  
                     concat [[Print e stmt|e<-el,stmt<-sl],[Assign x e s|x<-[X,Y],e<-el,s<-sl]]

instance Exhaustive Exp where
  exhaustive 0 = []
  exhaustive 1 = [Variable X, Variable Y, Value True, Value False]
  exhaustive n = let el = (exhaustive (n-1))::[Exp] in
                         concat[[And e1 e2|e1<-el,e2<-el],[Or e1 e2|e1<-el,e2<-el]]

take' :: Integer -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x:(take' (n-1) xs)
take' _ _ = []

valid:: ErrorOr a -> Bool
valid (Result _) = True
valid _ =  False

--interpret and compileSimulate
validate :: Height -> Quantity -> Bool
--validate n k = [((interpret stmt),(compileSimulate stmt),stmt) | stmt <- (take' k [s|s<-((exhaustive n)::[Stmt]), valid (typeCheck s)]), ((liftErr converts) (interpret stmt) )/= (compileSimulate stmt) ]
validate n k = and [((liftErr converts) (interpret stmt) )== (compileSimulate stmt) | stmt <- take' k [s|s<-((exhaustive n)::[Stmt]), valid (typeCheck s)]] 

--validate 4 10

complete :: String -> ErrorOr Buffer
complete _ = let x = tokenizeParse s in 
							 case x of 
									Result ast -> let y = typeCheck ast in
																	case y of 
																		Result _ -> compileSimulate ast
																		err -> promote err
									err -> promote err
									

--eof
