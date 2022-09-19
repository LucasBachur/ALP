module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple
import Data.Either (Either(Right, Left))

-- Estados
type State = M.Map Variable Int

-- Estado nulo
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case s M.!? v of
                  Just n -> Right n
                  _ -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do (c' :!: s') <- stepComm c s
                         stepCommStar c' s'
  
-- Evalua un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip s                     = Right (Skip :!: s)
stepComm (Let var exp) s            = case evalExp exp s of
                                        Right n -> Right (Skip :!: update var n s)
                                        Left error -> Left error
stepComm (Seq Skip c) s             = Right (c :!: s)
stepComm (Seq com1 com2) s          = case stepComm com1 s of
                                        Right (com :!: s') -> Right (Seq com com2 :!: s')
                                        Left error -> Left error
stepComm (IfThenElse bexp c1 c2) s  = case evalExp bexp s of
                                        Right b -> if b then Right (c1 :!: s)
                                                                 else Right (c2 :!: s)
                                        Left error -> Left error
stepComm loop@(While bexp c) s = Right ((IfThenElse bexp (Seq c loop) Skip):!: s)

-- Evalua una expresion
evalExp :: Exp a -> State -> Either Error a
evalExp (Const n) s         = Right n
evalExp (Var v) s           = lookfor v s
evalExp (UMinus exp) s      =  case evalExp exp s of
                                Right n -> Right (-n)
                                Left error -> Left error
evalExp (Plus exp1 exp2) s  = case evalExp exp1 s of
                                Right n1-> 
                                  case evalExp exp2 s of
                                      Right n2-> Right (n1 + n2)
                                      Left error -> Left error
                                Left error -> Left error
evalExp (Minus exp1 exp2) s = case evalExp exp1 s of
                                Right n1-> 
                                  case evalExp exp2 s of
                                    Right n2-> Right (n1 - n2)
                                    Left error -> Left error
                                Left error -> Left error
evalExp (Times exp1 exp2) s = case evalExp exp1 s of
                                Right n1 -> 
                                  case evalExp exp2 s of
                                    Right n2 -> Right (n1 * n2)
                                    Left error -> Left error
                                Left error -> Left error
evalExp (Div exp1 exp2) s   = case evalExp exp1 s of
                                Right n1 -> 
                                  case evalExp exp2 s of
                                    Right n2 -> 
                                      case n2 of
                                        0 -> Left DivByZero
                                        _ -> Right (div n1 n2)
                                    Left error -> Left error
                                Left error -> Left error
evalExp (ECond bexp exp1 exp2) s = case evalExp bexp s of
                                    Right b -> evalExp exp s
                                      where exp = if b then exp1 else exp2
                                    Left error -> Left error 
evalExp BTrue s           = Right True 
evalExp BFalse s          = Right False 
evalExp (Lt exp1 exp2) s  = case evalExp exp1 s of
                              Right b1  -> 
                                case evalExp exp2 s of
                                  Right b2 -> Right (b1 < b2) 
                                  Left error -> Left error
                              Left error -> Left error
evalExp (Gt exp1 exp2) s  = case evalExp exp1 s of
                              Right b1 -> 
                                case evalExp exp2 s of
                                  Right b2 -> Right (b1 > b2)
                                  Left error -> Left error
                              Left error -> Left error
evalExp (And exp1 exp2) s = case evalExp exp1 s of
                              Right b1 -> 
                                case evalExp exp2 s of
                                  Right b2 -> Right (b1 && b2)
                                  Left error -> Left error
                              Left error -> Left error
evalExp (Or exp1 exp2) s  = case evalExp exp1 s of
                              Right b1 -> 
                                case evalExp exp2 s of
                                  Right b2 -> Right (b1 || b2)
                                  Left error -> Left error
                              Left error -> Left error
evalExp (Not exp) s       = case evalExp exp s of
                              Right b -> Right (not b) 
                              Left error -> Left error
evalExp (Eq exp1 exp2) s  = case evalExp exp1 s of
                              Right b1 -> 
                                case evalExp exp2 s of
                                  Right b2 -> Right (b1 == b2)
                                  Left error -> Left error
                              Left error -> Left error 
evalExp (NEq exp1 exp2) s = case evalExp exp1 s of
                              Right b1 -> 
                                case evalExp exp2 s of
                                  Right b2 -> Right (b1 /= b2)
                                  Left error -> Left error
                              Left error -> Left error