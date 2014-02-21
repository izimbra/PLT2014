module Interpreter where

import Control.Monad
import System.Environment (getArgs)
import System.Exit (exitFailure)

import qualified Data.Map as M

import AbsCPP
import LexCPP
import ParCPP
import PrintCPP
import ErrM

import Environment

interpret :: Program -> IO ()
interpret (Prog defs) =  do iSigTab <-  buildSig M.empty defs 
                            iEnv <- return (iSigTab, [])  --why is return neeeded here? Is this a good way to write code?
                            execMain iEnv
--                           execFun (Id "main") iEnv
                          

execMain :: IEnv -> IO ()
execMain (iSigTab, conts) = case M.lookup (Id "main") iSigTab of
                                    Nothing -> error "no function main()" 
                                    Just (Fun _ _ _ stms)  -> do ienv <- execStms (iSigTab, conts) stms
                                                                 return ()


execFun :: Id -> IEnv -> [Value] -> IO()
execFun id (iSigTab, conts) vals = case M.lookup id iSigTab of
                                     Nothing -> error "function not found" -- should never happen since we have already typechecked, but case is needed because of lookup
                                     Just (Fun typ _ args stms) -> return ()
                                     --skapa context utifrån argumenten till funktionen
                                     --kör execStms
                                     --ska execFun returna ett IO Value?

buildSig :: ISigTab -> [Def] -> IO ISigTab
buildSig i [] = return i  --base case
buildSig iSigTab (d:ds) = buildSig (addFunSig iSigTab d) ds

addFunSig :: ISigTab -> Def -> ISigTab
addFunSig iSigTab (Fun t id a s) =  M.insert id (Fun t id a s) iSigTab


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
--evalExp env EId x   = evalVar env x 

-- literal values
evalExp env (ETrue )    = return (VInt 1,    env)
evalExp env (EFalse)    = return (VInt 0,    env)
evalExp env (EInt i)    = return (VInt i,    env)
evalExp env (EDouble d) = return (VDouble d, env)
-- variables
-- -- Err "uninitialized variable x"
-- unary operations w/side effects
evalExp env (EPIncr  e)  = undefined --case (evalExp env e) of
                          -- (VInt v) -> VInt (i1+1)
evalExp env (EPDecr  e)  = undefined
evalExp env (EIncr   e)  = undefined
evalExp env (EDecr   e)  = undefined
  
-- binary arithmetic operations
-- evalExp env (EPlus  e1 e2) = do  (v1,_) <- evalExp env e1
--                                  (v2,_) <- evalExp env e2
--                                  case (v1,v2) of
--                                   (VInt i1, VInt i2)       -> return (VInt (i1+i2), env)
--                                   (VDouble d1, VDouble d2) -> return (VDouble (d1+d2), env)
-- evalExp env (EMinus e1 e2) = let v1 = evalExp env e1
--                                  v2 = evalExp env e2
--                            in case (v1,v2) of
--                              (VInt i1, VInt i2)       -> return (VInt (i1-i2), env)
--                              (VDouble d1, VDouble d2) -> return (VDouble (d1-d2), env)
-- evalExp env (ETimes e1 e2) = let v1 = evalExp env e1
--                                  v2 = evalExp env e2
--                            in case (v1,v2) of
--                              (VInt i1, VInt i2)       -> return (VInt (i1*i2), env)
--                              (VDouble d1, VDouble d2) -> return (VDouble (d1*d2), env)
--                              -- add catch-all
-- evalExp env (EDiv  e1 e2)  = let v1 = evalExp env e1
--                                  v2 = evalExp env e2
--                            in case (v1,v2) of
--                              (VInt i1, VInt i2)       -> return (VDouble (i1/i2), env)
--                              (VDouble d1, VDouble d2) -> return (VDouble (d1/d2), env)
--                              -- add catch-all
-- comparison operators
evalExp env (ELt   e1 e2)     = undefined -- case evalPair (env e1 e2) of
                              --  (VInt i1,    VInt i2)    ->
                              --  (VInt i,     VDouble d)  ->
                              --  (VDouble d,  VInt i)     -> 
                              --  (VDouble d1, VDouble d2) ->
                                
evalExp env (EGt   e1 e2)     = undefined --do (v1,v2) <- getValuePair env e1 e2
evalExp env (ELtEq e1 e2)     = undefined
evalExp env (EGtEq e1 e2)     = undefined
evalExp env (EEq   e1 e2)     = undefined
evalExp env (ENEq  e1 e2)     = do (v1,v2) <- getValuePair env e1 e2
                                   return (some v1 v2, env)
   where
     some :: Value -> Value -> Value
     some a b = VInt 0

-- helper functions
-- | Extract values of a pair of expression, returns them in monadic 'IO' context.
-- Used to easier work with values in expressions that are free of side effects. 
getValuePair :: IEnv -> Exp -> Exp -> IO (Value, Value)
getValuePair env e1 e2 = do (v1,_) <- evalExp env e1
                            (v2,_) <- evalExp env e2
                            return (v1,v2)

-- | Compares two numeric values using given comparison function
-- and return @VInt 1@ for true or @VInt 0@ for false 
--compareValues :: (Ord a, Num a) => (Value, Value) ->  (a -> a -> Bool)  -> Value
--compareValues (v1, v2) cmp = case (v1, v2) of
--                                      boolToVal (i1 `cmp` i2)
--compareValues (VDouble d1, VDouble d2) cmp = boolToVal (d1 `cmp` d2)

--(v1,v2) comparison = do (v1,
-- type Env = [[(Ident, Value)]]

-- emptyEnv :: Env
-- emptyEnv = [[]]
