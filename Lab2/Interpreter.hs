module Interpreter where

import Control.Monad
import System.Environment (getArgs)
import System.Exit (exitFailure)

import AbsCPP
import LexCPP
import ParCPP
import PrintCPP
import ErrM

--data Value = VInt Integer | VDouble Double | VUndef

instance Show Value where
    show (VInt i)    = show i
    show (VDouble d) = show d
--    show (VBool b)   = show b
    show VUndef      = "undefined"

interpret :: Program -> IO ()
interpret (Prog stms) = do execStms emptyEnv stms
                           return ()

execStms :: Env -> [Stm] -> IO Env
execStms env [] = return env
execStms env (st:stms) = do env' <- execStm env st
                            execStms env' stms

execStm :: Env -> Stm -> IO Env
execStm env s = 
    case s of
      SDecl _ x       -> return (addVar env x)
      SAss x e        -> return (setVar env x (evalExp env e))
      SBlock stms     -> do env' <- execStms (enterScope env) stms
                            return (leaveScope env')
      SPrint e        -> do print (evalExp env e)
                            return env

evalExp :: Env -> Exp -> IO (Value, Env)
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
evalExp env EPDecr e     =
evalExp env EIncr e     =
evalExp env EDecr e     =
  
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
evalExp env ELt   e1 e2     =                              
evalExp env EGt   e1 e2     =
evalExp env ELtEq e1 e2     =
  
type Env = [[(Ident, Value)]]

emptyEnv :: Env
emptyEnv = [[]]

addVar :: Env -> Ident -> Env
addVar (scope:rest) x = (((x,VUndef):scope):rest)

setVar :: Env -> Ident -> Value -> Env
setVar [] x _ = error $ "Unknown variable " ++ printTree x ++ "."
setVar ([]:rest) x v = []:setVar rest x v
setVar ((p@(y,_):scope):rest) x v 
    | y == x = ((x,v):scope):rest
    | otherwise = let scope':rest' = setVar (scope:rest) x v
                   in (p:scope'):rest'

lookupVar :: Env -> Ident -> Value
lookupVar [] x = error $ "Unknown variable " ++ printTree x ++ "."
lookupVar (scope:rest) x = case lookup x scope of
                             Nothing -> lookupVar rest x
                             Just v  -> v
enterScope :: Env -> Env
enterScope env = []:env

leaveScope :: Env -> Env
leaveScope (_:env) = env
