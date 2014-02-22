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


--PASTE IN

interpret :: Program -> IO ()
interpret (Prog defs) =  do iSigTab <-  buildSig M.empty defs 
                            iEnv <- return (iSigTab, [])  --why is return neeeded here? Is this a good way to write code?
                            execMain iEnv
--                           execFun (Id "main") iEnv
                          

execMain :: IEnv -> IO ()
execMain (iSigTab, conts) = case M.lookup (Id "main") iSigTab of
                                    Nothing -> error "no function main()" --should not happen
                                    Just (Fun _ _ _ stms)  -> do ienv <- execStms (iSigTab, conts) stms
                                                                 return ()

--execfun in itself is not needed since it will not happen
--what will happen is a function call as a statement or as an expression
execFun :: Id -> IEnv -> [Value] -> IO()
execFun id (iSigTab, conts) vals = case M.lookup id iSigTab of
                                     Nothing -> error "function not found" -- should never happen since we have already typechecked, but case is needed because of lookup
                                     Just (Fun typ _ args stms) -> return ()
                                     --skapa context utifrån argumenten till funktionen
                                     --kör execStms
                                     --ska execFun returna ett IO Value?


-- STOP PASTE IN

execStms :: IEnv -> [Stm] -> IO IEnv
execStms env [] = return env
execStms env ((SReturn exp):stms) = do (v,env') <- evalExp env exp --evaluate exp
                                       return (setVar env' (Id "return") v) --sets the return variable and creates a new environment with that, which is returned to the caller who can take it out and continue
                                       
execStms env (st:stms) = do env' <- execStm env st
                            execStms env' stms
--checkStm :: Env -> Stm -> Err Env
execStm :: IEnv -> Stm -> IO IEnv
--checkStm env s = case s of
execStm env s = case s of
--   SDecl t x         -> updateVar env x t
	SDecl _ x          -> do --(funs, conts) <- return env --debug code with printing
                                 --print $ "before addVar: " ++ show conts
                                 --(funs', conts') <- return (addVar env x)
                                 --print $ "after addVar: " ++ show conts' 
                                 --return (funs', conts')-- end debug code with printing
                                 return  (addVar env x)  -- ORIGINAL CODE ONLY THIS LINE
--   SDecls _ x       -> return (addVar env x)
	SDecls _ xs        -> return (addVars env xs)


    --    SAss x e         -> return (setVar env x (evalExp env e)) --type error
        SBlock stms        -> do env' <- execStms (enterScope env) stms
                                 return (leaveScope env')
    --   SPrint e        -> do print (evalExp env e)
    --                         return env
	SAss x e           -> do   (v,env' ) <- evalExp env e
                                   return (setVar env' x v)
--                                 (funs, conts) <- return env  debug code with printing
--                                 print $ "before setVar: " ++ show conts
--                                 (v, env') <- evalExp env e
--                                 (funs', conts') <- return (setVar env' x v)
--                                 print $ "after setVar: " ++ show conts'
--                                 return (funs', conts') .. end debug code with printing

        SInit typ id exp   -> do env' <- execStm env (SDecl typ id)
                                 execStm env' (SAss id exp)
        SIfElse exp s1 s2  -> do --error ("execStm ifelse \n " ++ show exp ++ "\n" ++ show s1 ++ "\n" ++ show s2)
                                 --(b ,env') <- evalExp env exp
                                 --error ("execStm ifelse " ++ show b)
                                 ((VInt b) ,env') <- evalExp env exp
                                 if (b==1)
                                   then execStm env' s1
                                   else execStm env' s2
	--SReturn exp 	return statement has special treatment since it ends the execution of a series of statements, therefore pattern matching early in the function
        SExp exp            -> do (v,env') <- evalExp env exp
                                  return env'
                              
	_		-> error ("not finished yet in execStm: \n" ++ show s)
      



evalExp :: IEnv -> Exp -> IO (Value, IEnv)
-- literal values
evalExp env (ETrue )    = return (VInt 1,    env)
evalExp env (EFalse)    = return (VInt 0,    env)
evalExp env (EInt i)    = return (VInt i,    env)
evalExp env (EDouble d) = return (VDouble d, env)
-- variables, assignments, function calls
--evalExp env EId x       = evalVar env x 

evalExp env (EId id) = return (evalVar env id  , env)--error ("evalExp with id " ++ show id) -- 

-- -- Err "uninitialized variable x"
-- unary operations w/side effects   --temp commented out because it doesnt compile
--evalExp env (EPIncr  e)  = case (evalExp env e) of
--                             (VInt i) -> return (VInt (i+1),
--                                                 setVar env 
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
evalExp env (EEq   e1 e2)     = do --error (" evalExp EEq e1 e2 \n" ++ show e1 ++ "\n" ++ show e2)
                                   (v1,v2) <- getValuePair env e1 e2
                                   return ((compareValues (v1,v2) (==)),env)
evalExp env (ENEq  e1 e2)     = do (v1,v2) <- getValuePair env e1 e2
                                   return ((compareValues (v1,v2) (/=)),env)
--Calls of the four built-in functions can be hard-coded as special cases in the expression evaluation code. 
evalExp env (EApp (Id "printInt") [exp]) = do --(funs, conts) <- return env ..debug
                                              --print $ "evalExp printInt . " ++ show conts  ..debug
                                              (v, env') <- evalExp env exp
                                              print ( show v )
                                              return (VVoid, env') --printInt has type void
                                                     
                                                        
evalExp (sig, conts) (EApp fid a)      = do env' <- return (enterScope (sig, conts))
                                            case M.lookup fid sig of
                                                Just (Fun t id args stms) -> return (VUndef, env')
                                                Nothing -> error $ "function id not exist: " ++ show fid --should never happen

--evalExp (sig, conts) (EApp fid args)    = do --starta ett scope
--                                   env' <- enterScope (sig, conts) --env
                                   --hitta funktionen
--                                   error "troll"
--                                   (Fun t id args stms) <- M.lookup fid sig
                                   
                                   --initiera arg-variabler för funktionen i scopet
                                   --kör dess statements
--                                   return ( evalVar (Id "return") env'' , env'')
evalExp env e                 = error ("not finished yet in evalexp: \n" ++ show e)
-- helper functions
-- | Extract values of a pair of expression, returns them in monadic 'IO' context.
-- Used to easier work with values in expressions that are free of side effects. 

--How do you know if they are free of side effects? What about things like
--  if ( x = 5 < y = 3 )   
--  if ( x++  == y--   ) 
getValuePair :: IEnv -> Exp -> Exp -> IO (Value, Value)
getValuePair env e1 e2 = do --error ("getValuePair \n " ++ show e1 ++ "\n " ++ show e2 )
                            (v1,_) <- evalExp env e1
                            --error happens before this line
                            (v2,_) <- evalExp env e2
                            --error happens before this 
--                            error ("getValuePair \n " ++ show v1 ++ "\n " ++ show v2 )
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
