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
execStms :: IEnv -> [Stm] -> IO IEnv
execStms env [] = return env
execStms env ((SReturn exp):stms) = do (v,env') <- evalExp env exp --evaluate exp
                                       env'' <- return $ addVar env' (Id "return")
                                       return $ setVar env'' (Id "return") v --sets the return variable and creates a new environment with that, which is returned to the caller who can take it out and continue
                                       
execStms env (st:stms) = do env' <- execStm env st
                            execStms env' stms

execStm :: IEnv -> Stm -> IO IEnv
execStm env s = case s of
    SDecl _ x          -> return (addVar env x)  
    SDecls _ xs        -> return (addVars env xs)
    SAss x e           -> do let (funs, conts) = env
                             (v,env' ) <- evalExp env e
                             let (funs', conts') = env'
                             eee <- return (setVar env' x v)
                             let (funs'', conts'' ) = eee
                             return eee
    SBlock stms        -> do env' <- execStms (enterScope env) stms
                             return (leaveScope env')
    SInit typ id exp   -> do env' <- execStm env (SDecl typ id)
                             execStm env' (SAss id exp)
    SExp exp           -> do (v,env') <- evalExp env exp
                             return env'
    SIfElse exp s1 s2  -> do ((VInt b) ,env') <- evalExp env exp
                             if (b==1)
                                   then execStm env' s1
                                   else execStm env' s2
    SWhile exp stm     -> do let (funs, conts) = env 
                             ((VInt b) , env') <- evalExp env exp
                             let (f', c') = env'
                             if (b/=0)
                                   then do env'' <- execStm env' stm
                                           execStm env'' (SWhile exp stm)
                                   else return env'
    _       -> error ("not finished yet in execStm: \n" ++ show s)
      



evalExp :: IEnv -> Exp -> IO (Value, IEnv)
-- literal values
evalExp env (ETrue )    = return (VInt 1,    env)
evalExp env (EFalse)    = return (VInt 0,    env)
evalExp env (EInt i)    = return (VInt i,    env)
evalExp env (EDouble d) = return (VDouble d, env)

evalExp env (EId id) = return (evalVar env id  , env)

evalExp env (EPDecr (EId id)) = case (evalVar env id) of  --pattern matching directly on variable. The only cases we will get are for variables, as told in the instructions. And it could be changed in the grammar, but at this point feels too risky to change the grammar, so I made this shortcut
    (VInt i)    -> return (VInt i , setVar env id (VInt (i-1)))
    (VDouble i) -> return (VDouble i , setVar env id (VDouble (i-1)))

evalExp env (EPIncr (EId id)) = case (evalVar env id) of
    (VInt i)    -> return (VInt i , setVar env id (VInt (i+1)))
    (VDouble i) -> return (VDouble i , setVar env id (VDouble (i+1)))

evalExp env (EIncr (EId id)) = case (evalVar env id) of
    (VInt i)    -> return (VInt (i+1) , setVar env id (VInt (i+1)))
    (VDouble i) -> return (VDouble (i+1) , setVar env id (VDouble (i+1)))


evalExp env (EAss (EId id) e2) = do
    -- print $ "DP : evalExp EAss1 : " ++ show (EAss (EId id) e2)
    (v, env') <- evalExp env e2
    let (funs', conts') = env'
    -- print $ "DP : evalExp EAss2 (v, env') : " ++ show v ++ " , " ++ show conts'
    (v', env'') <- return (v, (setVar env' id v))
    let (funs'', conts'') =  env''
    -- print $ "DP : evalExp EAss3 :" ++ show v ++ " , " ++ show conts''
    return (v', env'')

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
        (VInt i, VDouble d)      -> return (VDouble (fromIntegral i - d), env)
        (VDouble d, VInt i)      -> return (VDouble (d - fromIntegral i), env)
        _                        -> error $ "Error EMinus non exhaustive case: " ++ show (EMinus e1 e2) ++ "    " ++ show v1 ++ "   " ++ show v2
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
      --  (VInt i1, VInt i2)       -> return (VDouble (fromInteger i1/fromInteger i2), env)
        (VInt i1, VInt i2)       -> return (VInt (floor (fromInteger i1/fromInteger i2)), env)
        (VDouble d1, VDouble d2) -> return (VDouble (d1/d2), env)
        (VDouble d,  VInt i)     -> return (VDouble (d / (fromIntegral i)),env)
        (VInt i, VDouble d)      -> return (VDouble (fromIntegral i / d), env)
        _                        -> error $ "Error EDiv non exhaustive case: " ++ show (EDiv e1 e2) ++ "    " ++ show v1 ++ "   " ++ show v2
                             -- add catch-all
-- comparison operators
--how can you skip evaluating the expressions and updating the environment here?
evalExp env (ELt   e1 e2)     = do --(v1,v2) <- getValuePair env e1 e2
                                   (vLeft, env') <- evalExp env e1
                                   (vRight, env'') <- evalExp env' e2
                                   
                                   return ((compareValues (vLeft,vRight) (<)),env'')
evalExp env (EGt   e1 e2)     = do --(v1,v2) <- getValuePair env e1 e2
                                   (vLeft, env') <- evalExp env e1
                                   (vRight, env'') <- evalExp env' e2
                                   return ((compareValues (vLeft,vRight) (>)),env'')
evalExp env (ELtEq e1 e2)     = do --(v1,v2) <- getValuePair env e1 e2
                                   (vLeft, env') <- evalExp env e1
                                   (vRight, env'') <- evalExp env' e2
                                   return ((compareValues (vLeft,vRight) (<=)),env'')
evalExp env (EGtEq e1 e2)     = do (vLeft, env') <- evalExp env e1
                                   (vRight, env'') <- evalExp env' e2
                                   return ((compareValues (vLeft,vRight) (>=)),env'')
evalExp env (EEq   e1 e2)     = do --error (" evalExp EEq e1 e2 \n" ++ show e1 ++ "\n" ++ show e2)
                                   --(v1,v2) <- getValuePair env e1 e2
                                   (vLeft, env') <- evalExp env e1
                                   (vRight, env'') <- evalExp env' e2
                                   return ((compareValues (vLeft,vRight) (==)),env'')
evalExp env (ENEq  e1 e2)     = do (vLeft, env') <- evalExp env e1
                                   (vRight, env'') <- evalExp env' e2
                                   return ((compareValues (vLeft,vRight) (/=)),env'')

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
--                                              let i = (\(VInt x)-> x) v
                                              print $ unwrapInt v
                                              return (VVoid, env') --printInt has type void
evalExp env (EApp (Id "printDouble") [exp]) = do
    (v,env') <- evalExp env exp
--    let d = (\(VDouble x) -> x) v
    print $ unwrapDouble v
    return (VVoid, env')                                                      
                                  
evalExp env (EApp (Id "readInt") exps) = do
    --print "Input an integer:"
    numberString <- getLine
    let number = read numberString
    -- print $ "debug print evalExp readInt : " ++ show (VInt number) 
    return (VInt number, env)                      

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



unwrapInt :: Value -> Integer
unwrapInt (VInt x) = x
unwrapInt (VDouble x) = floor x
unwrapInt e = error $ "troll unwrap on integer failed because: " ++  show e

unwrapDouble :: Value -> Double
unwrapDouble (VDouble x) = x
unwrapDouble e = error $ "troll unwrap on double failed because: " ++  show e


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
                            -- print $ "debug print valuepair v1 : " ++ show v1
                            --error happens before this line
                            (v2,_) <- evalExp env e2
                            -- print $ "debug print valuepair v2 : " ++ show v2


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



-- For good practice, the code could (should) be rewritten using let for all pure bindings
--http://learnyouahaskell.com/input-and-output
--
--import Data.Char  
--  
--main = do  
--    putStrLn "What's your first name?"  
--    firstName <- getLine  
--    putStrLn "What's your last name?"  
--    lastName <- getLine  
--    let bigFirstName = map toUpper firstName  
--        bigLastName = map toUpper lastName  
--    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"  
--See how the I/O actions in the do block are lined up? Also notice how the let is lined up with the I/O actions and the -names of the let are lined up with each other? That's good practice, because indentation is important in Haskell. Now,- we did map toUpper firstName, which turns something like "John" into a much cooler string like "JOHN". We bound that- uppercased string to a name and then used it in a string later on that we printed to the terminal.-

--You may be wondering when to use <- and when to use let bindings? Well, remember, <- is (for now) for performing I/O actions and binding their results to names. map toUpper firstName, however, isn't an I/O action. It's a pure expression in Haskell. So use <- when you want to bind results of I/O actions to names and you can use let bindings to bind pure expressions to names. Had we done something like let firstName = getLine, we would have just called the getLine I/O action a different name and we'd still have to run it through a <- to perform it.
--
