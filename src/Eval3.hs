module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = (M.Map Variable Int, Integer)

-- Estado nulo
-- Completar la definición
initState :: State
initState = (M.empty, 0)

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v (s, w) = case s M.!? v of
                  Just n -> Right n
                  _ -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update x v (s, w) = (M.insert x v s, w)

-- Suma un costo dado al estado
-- Completar la definición
addWork :: Integer -> State -> State
addWork n = (\(x,y) -> (x, y + n))

-- Suma los costos de 2 estados, cuando los estados son iguales.
sumWorks :: State -> State -> State
sumWorks (s, w1) (s', w2) = (s, w1 + w2)

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip s                     = Right (Skip :!: s)
stepComm (Let var exp) (s,w)            = case evalExp exp (s,0) of
                                            Right (n :!: s') -> Right (Skip :!: (sumWorks (update var n (s,w)) s'))
                                            Left error -> Left error
stepComm (Seq Skip c) s             = Right (c :!: s)
stepComm (Seq com1 com2) s          = case stepComm com1 s of
                                        Right (com :!: s') -> Right (Seq com com2 :!: s')
                                        Left error -> Left error
stepComm (IfThenElse bexp c1 c2) (s,w)  = case evalExp bexp (s,0) of
                                            Right (b :!: s') -> let c = if b then c1 else c2
                                                                  in Right (c :!: (addWork w s'))
                                            Left error -> Left error
stepComm loop@(While bexp c) s = Right ((IfThenElse bexp (Seq c loop) Skip):!: s)

-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s         = Right (n :!: s)
evalExp (Var v) s           = case lookfor v s of
                                Right n -> Right (n :!: s)
                                Left UndefVar -> Left UndefVar

evalExp (UMinus exp) s      =  case evalExp exp s of
                                Right (n :!: s') -> Right (-n :!: (addWork 1 s'))
                                Left error -> Left error
evalExp (Plus exp1 exp2) s  = case evalExp exp1 s of
                                    Right (n1 :!: s')-> 
                                      case evalExp exp2 s of
                                          Right (n2 :!: s'')-> Right ((n1 + n2) :!: (addWork 2 (sumWorks s' s'')))
                                          Left error -> Left error
                                    Left error -> Left error
evalExp (Minus exp1 exp2) s = case evalExp exp1 s of
                                Right (n1 :!: s')-> 
                                  case evalExp exp2 s of
                                    Right (n2 :!: s'')-> Right ((n1 - n2) :!: (addWork 2 (sumWorks s' s'')))
                                    Left error -> Left error
                                Left error -> Left error
evalExp (Times exp1 exp2) s = case evalExp exp1 s of
                                Right (n1 :!: s') -> 
                                  case evalExp exp2 s of
                                    Right (n2 :!: s'') -> Right ((n1 * n2) :!: (addWork 3 (sumWorks s' s'')))
                                    Left error -> Left error
                                Left error -> Left error
evalExp (Div exp1 exp2) s   = case evalExp exp1 s of
                                Right (n1 :!: s') -> 
                                  case evalExp exp2 s of
                                    Right (n2 :!: s'') -> 
                                      case n2 of
                                        0 -> Left DivByZero
                                        _ -> Right ((div n1 n2) :!: (addWork 3 (sumWorks s' s'')))
                                    Left error -> Left error
                                Left error -> Left error
evalExp (ECond bexp exp1 exp2) s = case evalExp bexp s of
                                    Right (b :!: s') -> 
                                      case evalExp exp s of
                                        Right (n :!: s'') -> Right (n :!: (sumWorks s' s''))
                                        Left error -> Left error 
                                      where exp = if b then exp1 else exp2
                                    Left error -> Left error 
evalExp BTrue s           = Right (True :!: s) 
evalExp BFalse s          = Right (False :!: s) 
evalExp (Lt exp1 exp2) s  = case evalExp exp1 s of
                              Right (b1 :!: s') -> 
                                case evalExp exp2 s of
                                  Right (b2 :!: s'') -> Right ((b1 < b2) :!: (addWork 2 (sumWorks s' s''))) 
                                  Left error -> Left error
                              Left error -> Left error
evalExp (Gt exp1 exp2) s  = case evalExp exp1 s of
                              Right (b1 :!: s') -> 
                                case evalExp exp2 s of
                                  Right (b2 :!: s'') -> Right ((b1 > b2) :!: (addWork 2 (sumWorks s' s''))) 
                                  Left error -> Left error
                              Left error -> Left error
evalExp (And exp1 exp2) s = case evalExp exp1 s of
                              Right (b1 :!: s') -> 
                                case evalExp exp2 s of
                                  Right (b2 :!: s'') -> Right ((b1 && b2) :!: (addWork 2 (sumWorks s' s''))) 
                                  Left error -> Left error
                              Left error -> Left error
evalExp (Or exp1 exp2) s  = case evalExp exp1 s of
                              Right (b1 :!: s') -> 
                                case evalExp exp2 s of
                                  Right (b2 :!: s'') -> Right ((b1 || b2) :!: (addWork 2 (sumWorks s' s'')))
                                  Left error -> Left error
                              Left error -> Left error
evalExp (Not exp) s       = case evalExp exp s of
                              Right (b :!: s') -> Right (not b :!: (addWork 1 s')) 
                              Left error -> Left error
evalExp (Eq exp1 exp2) s  = case evalExp exp1 s of
                              Right (b1 :!: s') -> 
                                case evalExp exp2 s of
                                  Right (b2 :!: s'') -> Right ((b1 == b2) :!: (addWork 2 (sumWorks s' s'')))
                                  Left error -> Left error
                              Left error -> Left error 
evalExp (NEq exp1 exp2) s = case evalExp exp1 s of
                              Right (b1 :!: s') -> 
                                case evalExp exp2 s of
                                  Right (b2 :!: s'') -> Right ((b1 /= b2) :!: (addWork 2 (sumWorks s' s'')))
                                  Left error -> Left error
                              Left error -> Left error
