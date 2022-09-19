module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple
import Text.ParserCombinators.ReadPrec (step)
import GHC.Base (seq)

-- Estados
type State = M.Map Variable Int

-- Estado nulo
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v s = s M.! v

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s= (Skip :!: s)
stepComm (Let var exp) s = let n = evalExp exp s
                           in (Skip :!: update var n s)
stepComm (Seq Skip c) s = (c :!: s)
stepComm (Seq com1 com2) s = let (com :!: s') = stepComm com1 s
                             in (Seq com com2 :!: s')
stepComm (IfThenElse bexp c1 c2) s = let b = evalExp bexp s
                                     in if b then (c1 :!: s) else (c2 :!: s)
stepComm loop@(While bexp c) s = ((IfThenElse bexp (Seq c loop) Skip) :!: s)

-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> a
evalExp (Const n) s = n
evalExp (Var v) s = lookfor v s
evalExp (UMinus exp) s = let n = evalExp exp s in -n
evalExp (Plus exp1 exp2) s = let n1 = evalExp exp1 s
                                 n2 = evalExp exp2 s
                             in n1 + n2
evalExp (Minus exp1 exp2) s = let n1 = evalExp exp1 s
                                  n2 = evalExp exp2 s
                              in n1 - n2
evalExp (Times exp1 exp2) s = let n1 = evalExp exp1 s
                                  n2 = evalExp exp2 s
                              in n1 * n2
evalExp (Div exp1 exp2) s = let n1 = evalExp exp1 s
                                n2 = evalExp exp2 s
                            in div n1 n2
evalExp (ECond bexp exp1 exp2) s = let b = evalExp bexp s
                                   in if b then evalExp exp1 s else evalExp exp2 s

evalExp BTrue s = True
evalExp BFalse s = False
evalExp (Lt exp1 exp2) s = let b1 = evalExp exp1 s
                               b2 = evalExp exp2 s
                           in b1 < b2
evalExp (Gt exp1 exp2) s = let b1 = evalExp exp1 s
                               b2 = evalExp exp2 s
                           in b1 > b2 
evalExp (And exp1 exp2) s = let b1 = evalExp exp1 s
                                b2 = evalExp exp2 s
                           in (b1 && b2)
evalExp (Or exp1 exp2) s = let b1 = evalExp exp1 s
                               b2 = evalExp exp2 s
                           in (b1 || b2)
evalExp (Not exp) s = let b = evalExp exp s
                      in not b
evalExp (Eq exp1 exp2) s = let b1 = evalExp exp1 s
                               b2 = evalExp exp2 s
                           in b1 == b2
evalExp (NEq exp1 exp2) s = let b1 = evalExp exp1 s
                                b2 = evalExp exp2 s
                            in b1 /= b2