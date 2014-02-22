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
                                    Just (Fun _ _ _ stms)  -> do --print $ show (iSigTab, conts)
                                                                 ienv <- execStms (iSigTab, conts) stms
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
                                       env'' <- return $ addVar env' (Id "return")
                                       return $ setVar env'' (Id "return") v --sets the return variable and creates a new environment with that, which is returned to the caller who can take it out and continue
                                       
execStms env (st:stms) = do env' <- execStm env st
                            execStms env' stms
--checkStm :: Env -> Stm -> Err Env
execStm :: IEnv -> Stm -> IO IEnv
--checkStm env s = case s of
execStm env s = case s of
	SDecl _ x          -> return  (addVar env x)  -- ORIGINAL CODE ONLY THIS LINE
	SDecls _ xs        -> return (addVars env xs)
        SBlock stms        -> do env' <- execStms (enterScope env) stms
                                 return (leaveScope env')
	SAss x e           -> do   (v,env' ) <- evalExp env e
                                   return (setVar env' x v)
        SInit typ id exp   -> do env' <- execStm env (SDecl typ id)
                                 execStm env' (SAss id exp)
        SExp exp           -> do (v,env') <- evalExp env exp
                                 return env'
        SIfElse exp s1 s2  -> do ((VInt b) ,env') <- evalExp env exp
                                 if (b==1)
                                   then execStm env' s1
                                   else execStm env' s2
        SWhile exp stm     -> do ((VInt b) , env') <- evalExp env exp
                                 if (b==1)
                                   then do env'' <- execStm env' stm
                                           execStm env'' (SWhile exp stm)
                                   else return env'

    --   SPrint e        -> do print (evalExp env e)
    --                         return env
	--SReturn exp 	return statement has special treatment in execStms
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

evalExp env (EPDecr (EId id)) = case (evalVar env id) of  --pattern matching directly on variable. The only cases we will get are for variables, as told in the instructions. And it could be changed in the grammar, but at this point feels too risky to change the grammar, so I made this shortcut
    --(vtyp i) -> return (vtyp i , setVar env id (vtyp (i+1))) --this doesn't work, but how can it be made to work? would be nice
    (VInt i)    -> return (VInt i , setVar env id (VInt (i-1)))
    (VDouble i) -> return (VDouble i , setVar env id (VDouble (i-1)))

--evalExp env (EPDecr id) = return ((vtyp, v), env') 
--    where 
--        (vtyp, v) = evalVar env id
--        env' = setVar env id (vtyp (v+1))
--case (evalVar env id) of
  --  (VInt i)    -> 
    --(VDouble i) ->    
-- -- Err "uninitialized variable x"
-- unary operations w/side effects   --temp commented out because it doesnt compile
--evalExp env (EPIncr  e)  = case (evalExp env e) of
--                             (VInt i) -> return (VInt (i+1),
--                                                 setVar env 
--evalExp env (EPDecr  e)  = undefined
--evalExp env (EIncr   e)  = undefined
--evalExp env (EDecr   e)  = undefined
  
-- binary arithmetic operations
evalExp env (EPlus  e1 e2) = do 
    (v1,v2) <- getValuePair env e1 e2
    case (v1,v2) of
        (VInt i1, VInt i2)       -> return (VInt (i1+i2), env)
        (VDouble d1, VDouble d2) -> return (VDouble (d1+d2), env)
        (VInt i, VDouble d)      -> return (VDouble (fromIntegral i + d), env)
        (VDouble d, VInt i)      -> return (VDouble (d + fromIntegral i), env)
        _                        -> error $ "Error EPlus non exhaustive case: " ++ show (EPlus e1 e2) ++ "    " ++ show v1 ++ "   " ++ show v2

evalExp env (EMinus e1 e2) = do 
    (v1,v2) <- getValuePair env e1 e2
    case (v1,v2) of
        (VInt i1, VInt i2)       -> return (VInt (i1-i2), env)
        (VDouble d1, VDouble d2) -> return (VDouble (d1-d2), env)

evalExp env (ETimes e1 e2) = do 
    (v1,v2) <- getValuePair env e1 e2
    case (v1,v2) of
        (VInt i1, VInt i2)       -> return (VInt (i1*i2), env)
        (VDouble d1, VDouble d2) -> return (VDouble (d1*d2), env)
        (VInt i, VDouble d)      -> return (VDouble (fromIntegral i * d), env)
        (VDouble d, VInt i)      -> return (VDouble (d * fromIntegral i), env)
        _                        -> error $ "Error ETimes non exhaustive case: " ++ show (ETimes e1 e2) ++ "    " ++ show v1 ++ "   " ++ show v2
                             -- add catch-all ?
evalExp env (EDiv  e1 e2)  = do 
    (v1,v2) <- getValuePair env e1 e2
    case (v1,v2) of
        (VInt i1, VInt i2)       -> return (VDouble (fromInteger i1/fromInteger i2), env)
        (VDouble d1, VDouble d2) -> return (VDouble (d1/d2), env)
        (VDouble d,  VInt i)     -> return (VDouble (d / (fromIntegral i)),env)
        _                        -> error $ "Error EDiv non exhaustive case: " ++ show (EDiv e1 e2) ++ "    " ++ show v1 ++ "   " ++ show v2
                             -- add catch-all
-- comparison operators
--how can you skip evaluating the expressions and updating the environment here?
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

--- same but different
evalExp env (EAnd e1 e2)      = do    --remember evaluation order! we cannot generalise too much
    ((VInt b1) , env')  <- evalExp env  e1
    if (b1 == 0)
        then return ((VInt 0) , env')
        else do
            ((VInt b2), env'') <- evalExp env' e2
            if (b2 == 0)
                then return ((VInt 0), env'')
                else return ((VInt 1), env'')

evalExp env (EOr e1 e2)       = do --remember evaluation order! we cannot generalise too much
    ((VInt b1) , env')  <- evalExp env  e1
    if (b1 == 1) 
        then return ((VInt 1), env')
        else do
            ((VInt b2) , env'') <- evalExp env' e2
            if (b2 == 1)
                then return ((VInt 1) , env'')
                else return ((VInt 0) , env'') 

-- SIfElse exp s1 s2  -> do ((VInt b) ,env') <- evalExp env exp
--                                 if (b==1)
--                                   then execStm env' s1
--                                   else execStm env' s2


--Calls of the four built-in functions can be hard-coded as special cases in the expression evaluation code. 
evalExp env (EApp (Id "printInt") [exp]) = do --(funs, conts) <- return env ..debug
                                              --print $ "evalExp printInt . " ++ show conts  ..debug
                                              (v, env') <- evalExp env exp
                                              print ( show v )
                                              return (VVoid, env') --printInt has type void
evalExp env (EApp (Id "printDouble") [exp]) = do
    (v,env') <- evalExp env exp
    print (show v)
    return (VVoid, env')                                                      
                                                        
evalExp (sig, conts) (EApp callId callArgExps) = case M.lookup callId sig of
    Nothing -> error $ "function id not exist: " ++ show callId --should never 
    Just (Fun t id args stms) -> do --jag har en funktion
        --extrahera variabelNamn
        argIds <- return (map (\(Arg t id) -> id) args)
        --evaluera alla arguments
        (env' , callValues) <- ( evalArgs (sig, conts) callArgExps [])
        callValues' <- return (reverse callValues) -- fix order of arguments
        env'' <- return (addVars (enterScope env') argIds)
        --sätt variabelvärden
        env''' <- return ( setVars env'' argIds callValues')
        env'''' <- execStms env''' stms                                       --kör statements
        returnValue <- return (evalVar env''''  (Id "return"))                          --ta hand om returvärde
        return (returnValue, leaveScope env'''')
                                                                                --en funktion ses inte som ett block i normal mening fast den kanske kunde gjort det. 

evalExp env e                 = error ("not finished yet in evalexp: \n" ++ show e)
-- helper functions
-- | Extract values of a pair of expression, returns them in monadic 'IO' context.
-- Used to easier work with values in expressions that are free of side effects. 

evalArgs :: IEnv -> [Exp] -> [Value] -> IO (IEnv, [Value])  --Helper function to evaluate arguments in a function call. must be called with an empty value list from outside
evalArgs env [] vs = return (env, vs)  --base case, when there are no more args (exps) to evaluate
evalArgs env (e:es) vs = do --warning : list of values will be reversed. will be reversed before use in the evalExp code
        (v, env') <- evalExp env e
        evalArgs env' es (v:vs)

--evalArgs env' es (v:vs)
--    where (v, env') = return (evalExp env e )

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
