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



--type Env = [[(Id, Value)]]   --OLD DEFINITION
--emptyEnv :: Env
--emptyEnv = [[]]

--type IEnv = (SigTab, [IContext])  --NEW DEFINITION
--type SigTab = M.Map Id Sig
-- | Function type signature. Includes argument types and return type.
--type Sig = ([Type], Type)



instance Show Value where
    show (VInt i)    = show i
    show (VDouble d) = show d
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

evalExp :: IEnv -> Exp -> Value
evalExp env e = 
    case e of
      EId x         -> lookupVar env x
      EInt i         -> VInt i
      EDouble d      -> VDouble d
      EPlus e1 e2     -> let v1 = evalExp env e1
                             v2 = evalExp env e2
                         in case (v1,v2) of
                              (VInt i1, VInt i2)       -> VInt (i1+i2)
                              (VDouble d1, VDouble d2) -> VDouble (d1+d2)


addVar :: IEnv -> Id -> IEnv
addVar (scope:rest) x = (((x,VUndef):scope):rest)

setVar :: IEnv -> Id -> Value -> IEnv
setVar [] x _ = error $ "Unknown variable " ++ printTree x ++ "."
setVar ([]:rest) x v = []:setVar rest x v
setVar ((p@(y,_):scope):rest) x v 
    | y == x = ((x,v):scope):rest
    | otherwise = let scope':rest' = setVar (scope:rest) x v
                   in (p:scope'):rest'

lookupVar :: IEnv -> Id -> Value
lookupVar [] x = error $ "Unknown variable " ++ printTree x ++ "."
lookupVar (scope:rest) x = case lookup x scope of
                             Nothing -> lookupVar rest x
                             Just v  -> v
enterScope :: IEnv -> IEnv
enterScope env = []:env

leaveScope :: IEnv -> IEnv
leaveScope (_:env) = env
