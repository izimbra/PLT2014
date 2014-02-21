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

interpret :: Program -> IO ()
interpret (Prog defs) = --do execStms emptyEnv stms
                           return ()

execStms :: IEnv -> [Stm] -> IO IEnv
execStms env [] = return env
execStms env (st:stms) = do env' <- execStm env st
                            execStms env' stms

execStm :: IEnv -> Stm -> IO IEnv
execStm env s = undefined
    -- case s of
    --   SDecls _ x       -> return (addVar env x)
    --   SAss x e        ->  return (setVar env x (evalExp env e))
    --   SBlock stms     -> do env' <- execStms (enterScope env) stms
    --                         return (leaveScope env')
    --   SPrint e        -> do print (evalExp env e)
    --                         return env

evalExp :: IEnv -> Exp -> IO (Value, IEnv)
-- literal values
evalExp env (ETrue )    = return (VInt 1,    env)
evalExp env (EFalse)    = return (VInt 0,    env)
evalExp env (EInt i)    = return (VInt i,    env)
evalExp env (EDouble d) = return (VDouble d, env)
-- variables, assignments, function calls
--evalExp env EId x       = evalVar env x 

-- -- Err "uninitialized variable x"
-- unary operations w/side effects
evalExp env (EPIncr  e)  = case (evalExp env e) of
                             (VInt i) -> return (VInt (i+1),
                                                 setVar env 
evalExp env (EPDecr  e)  = undefined
evalExp env (EIncr   e)  = undefined
evalExp env (EDecr   e)  = undefined
  
-- binary arithmetic operations
evalExp env (EPlus  e1 e2) = do (v1,v2) <- getValuePair env e1 e2
                                case (v1,v2) of
                                  (VInt i1, VInt i2)       -> return (VInt (i1+i2), env)
                                  (VDouble d1, VDouble d2) -> return (VDouble (d1+d2), env)
evalExp env (EMinus e1 e2) = do (v1,v2) <- getValuePair env e1 e2
                                case (v1,v2) of
                                  (VInt i1, VInt i2)       -> return (VInt (i1-i2), env)
                                  (VDouble d1, VDouble d2) -> return (VDouble (d1-d2), env)
evalExp env (ETimes e1 e2) = do (v1,v2) <- getValuePair env e1 e2
                                case (v1,v2) of
                                  (VInt i1, VInt i2)       -> return (VInt (i1*i2), env)
                                  (VDouble d1, VDouble d2) -> return (VDouble (d1*d2), env)
                             -- add catch-all ?
evalExp env (EDiv  e1 e2)  = do (v1,v2) <- getValuePair env e1 e2
                                case (v1,v2) of
                                  (VInt i1, VInt i2)       -> return (VDouble (
                                                                         fromInteger i1/
                                                                         fromInteger i2), env)
                                  (VDouble d1, VDouble d2) -> return (VDouble (d1/d2), env)
                             -- add catch-all
-- comparison operators
evalExp env (ELt   e1 e2)     = do (v1,v2) <- getValuePair env e1 e2
                                   return ((compareValues (v1,v2) (<)),env)
evalExp env (EGt   e1 e2)     = do (v1,v2) <- getValuePair env e1 e2
                                   return ((compareValues (v1,v2) (>)),env)
evalExp env (ELtEq e1 e2)     = do (v1,v2) <- getValuePair env e1 e2
                                   return ((compareValues (v1,v2) (<=)),env)
evalExp env (EGtEq e1 e2)     = do (v1,v2) <- getValuePair env e1 e2
                                   return ((compareValues (v1,v2) (>=)),env)
evalExp env (EEq   e1 e2)     = do (v1,v2) <- getValuePair env e1 e2
                                   return ((compareValues (v1,v2) (==)),env)
evalExp env (ENEq  e1 e2)     = do (v1,v2) <- getValuePair env e1 e2
                                   return ((compareValues (v1,v2) (/=)),env)

-- helper functions
-- | Extract values of a pair of expression, returns them in monadic 'IO' context.
-- Used to easier work with values in expressions that are free of side effects. 
getValuePair :: IEnv -> Exp -> Exp -> IO (Value, Value)
getValuePair env e1 e2 = do (v1,_) <- evalExp env e1
                            (v2,_) <- evalExp env e2
                            return (v1,v2)

-- | Compares two numeric values using given comparison function
-- and return @VInt 1@ for true or @VInt 0@ for false.
-- Returns @VUndef@ if comparison fails.                            
compareValues (v1, v2) cmp = case (v1, v2) of
                               (VInt i1,    VInt i2)     -> boolToVal (fromInteger i1
                                                                       `cmp`
                                                                       fromInteger i2)
                               (VDouble d1, VDouble d2)  -> boolToVal (d1
                                                                       `cmp`
                                                                       d2)
                               (VDouble d,  VInt i)      -> boolToVal (d  `cmp`
                                                                       fromInteger i )
                               (VInt i,     VDouble d )  -> boolToVal (fromInteger i
                                                                       `cmp` d  )

-- | Applies boolean operator to values constructed using @VBool@.
applyBool :: Value -> (Bool -> Bool ->  Bool) -> Value -> Value
applyBool v1 boolOp v2 = boolToVal $ (valToBool v1) `boolOp` (valToBool v2)

--compareValues (VDouble d1, VDouble d2) cmp = boolToVal (d1 `cmp` d2)

--(v1,v2) comparison = do (v1,
-- type Env = [[(Ident, Value)]]

-- emptyEnv :: Env
-- emptyEnv = [[]]
