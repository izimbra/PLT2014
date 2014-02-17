module Interpreter where

import Control.Monad
import System.Environment (getArgs)
import System.Exit (exitFailure)

import AbsCPP
import LexCPP
import ParCPP
import PrintCPP
import ErrM

import Environment

instance Show Value where
    show (VInt i)    = show i
    show (VDouble d) = show d
--    show (VBool b)   = show b
    show VUndef      = "undefined"

interpret :: Program -> IO ()
interpret (Prog stms) = do execStms emptyEnv stms
                           return ()

execStms :: IEnv -> [Stm] -> IO IEnv
execStms env [] = return env
execStms env (st:stms) = do env' <- execStm env st
                            execStms env' stms

execStm :: IEnv -> Stm -> IO IEnv
execStm env s = 
    case s of
      SDecl _ x       -> return (addVar env x)
      SAss x e        -> return (setVar env x (evalExp env e))
      SBlock stms     -> do env' <- execStms (enterScope env) stms
                            return (leaveScope env')
      SPrint e        -> do print (evalExp env e)
                            return env

evalExp :: IEnv -> Exp -> IO (Value, Env)
--evalExp env EId x   = lookupVar env x



-- literal values
evalExp env ETrue     = return (VInt 1, env)
evalExp env EFalse    = return (VInt 0, env)
evalExp env EInt i    = return (VInt i, env)
evalExp env EDouble d = return (VDouble d, env)
-- variables
-- -- Err "uninitialized variable x"
-- unary operations w/side effects
evalExp env EPIncr e     = case (evalExp env e) of
                            (VInt v) -> VInt (i1+1)
evalExp env EPDecr e     = undefined
evalExp env EIncr  e     = undefined
evalExp env EDecr  e     = undefined
  
-- binary arithmetic operations
evalExp env EPlus  e1 e2 = let v1 = evalExp env e1
                               v2 = evalExp env e2
                           in case (v1,v2) of
                             (VInt i1, VInt i2)       -> return (VInt (i1+i2), env)
                             (VDouble d1, VDouble d2) -> return (VDouble (d1+d2), env)
evalExp env EMinus e1 e2 = let v1 = evalExp env e1
                               v2 = evalExp env e2
                           in case (v1,v2) of
                             (VInt i1, VInt i2)       -> return (VInt (i1-i2), env)
                             (VDouble d1, VDouble d2) -> return (VDouble (d1-d2), env)
evalExp env ETimes e1 e2 = let v1 = evalExp env e1
                               v2 = evalExp env e2
                           in case (v1,v2) of
                             (VInt i1, VInt i2)       -> return (VInt (i1*i2), env)
                             (VDouble d1, VDouble d2) -> return (VDouble (d1*d2), env)
                             -- add catch-all
evalExp env EDiv   e1 e2 = let v1 = evalExp env e1
                               v2 = evalExp env e2
                           in case (v1,v2) of
                             (VInt i1, VInt i2)       -> return (VDouble (i1/i2), env)
                             (VDouble d1, VDouble d2) -> return (VDouble (d1/d2), env)
                             -- add catch-all
-- comparison operators
evalExp env ELt   e1 e1     = undefined
evalExp env EGt   e1 e2     = undefined
evalExp env ELtEq e1 e2     = undefined
  
-- type Env = [[(Ident, Value)]]

-- emptyEnv :: Env
-- emptyEnv = [[]]
