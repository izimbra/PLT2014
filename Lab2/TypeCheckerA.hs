-- | Annotating type checker
module TypeCheckerA where

import Control.Monad(mapAndUnzipM,zipWithM)

import AbsCPP
import PrintCPP
import ErrM
import BuiltIns
import Environment

-- 2) do type-checking and annotation
-- check EApp f es 
--  (ts, t) <- lookUp f
--  smart way is to use zip:
--  let ets = zip es ts - but will truncate the longer list!
--  mapM (\(e,t) -> check e t) ets

typecheck :: Program -> Err Program
typecheck (Prog defs) = do
            env <- buildFunTable emptyEnvT (defs ++ builtInFunctions)
            (defs',_) <- checkDefs env defs
            return (Prog defs')

-- | Builds a symbol table for functions in the environment.
buildFunTable :: EnvT -> [Def] -> Err EnvT -- or just SigTab
buildFunTable env []   = return env
buildFunTable env (d:ds) =
    case d of
      Fun ftype id args _ -> do env' <- updateFunT env id (map argType args, ftype)
                                buildFunTable env' ds                                 
      _                    -> fail "Bad function definition, buildFunTable"
    
-- | Type-checks a list of function definitions.
checkDefs :: EnvT -> [Def] -> Err ([Def],())
checkDefs env []     = return ([],())
checkDefs env (d:ds) = do (d' ,env' ) <- checkDef  env  d
                          (ds',env'') <- checkDefs env' ds
                          return(d':ds', env'')

-- | Type-checks a function definition.
checkDef :: EnvT -> Def -> Err (Def, EnvT)
checkDef env (Fun t id args stms) = do env'  <- addArgsT  (addScopeT env) args
                                       env'' <- updateVarT env' (Id "return") t
--since return i a reserved word, there will never be a variable with that as id
--so we can use it to store the function type in every scope
                                       (stms',env''') <- checkStms env'' stms
                                       return (Fun t id args stms',env''')

checkStms :: EnvT -> [Stm] -> Err ([Stm],EnvT)
checkStms env [] = return ([],env)
checkStms env (st:stms) = do
  (st'  ,env' ) <- checkStm  env  st
  (stms',env'') <- checkStms env' stms
  return (st':stms',env'')
  
checkStm :: EnvT -> Stm -> Err (Stm,EnvT)
checkStm env s = 
    case s of

      SDecl t x         -> do env' <- updateVarT env x t
                              return(s,env')
      SDecls t ids       -> do (_, envs) <- mapAndUnzipM (checkOne env t) ids
                               return (SDecls t ids, last envs)
                                 where checkOne :: EnvT -> Type -> Id -> Err (Stm,EnvT)
                                       checkOne env t id = checkStm env (SDecl t id)
      SAss  x e         -> do t  <- lookupVarT env x
                              e' <- checkExp env e t
                              return (SAss x e',env)      
      SInit t id exp    -> do -- first declare
                              (_,env')     <- checkStm env (SDecl t id)
                              -- then check assignment
                              (SAss id exp',env'') <- checkStm env' (SAss id exp)
                              return (SInit t id exp', env'')
      SBlock stms       -> do (stms',_) <- checkStms (addScopeT env) stms
                              return (SBlock stms',env) -- or env'?
      SReturn exp       -> do retType <- lookupVarT env (Id "return") 
                              exp' <- checkExp env exp retType
                              return (SReturn exp', env)
      SExp exp          -> do (exp',t) <- inferExp env exp
                              return (SExp exp', env)
      SIfElse exp s1 s2 -> do exp'        <- checkExp env exp TBool
                              (s1',env' ) <- checkStm env s1
                              (s2',env'') <- checkStm env s2
                              return (SIfElse exp' s1' s2', env'')
      SWhile exp stm    -> do exp' <- checkExp env exp TBool
                              (stm',env')  <- checkStm env stm
                              return (SWhile exp' stm', env')
      
      --updateVars env ids typ
      _                 -> error ("Case not exhaustive in checkstm  \n"
                                  ++ show s ++ " \n  " ++ printTree s) 

-- | Checks type of the expression argument.
checkExp :: EnvT -> Exp -> Type -> Err Exp
checkExp env e t = 
    do (e',t') <- inferExp env e
       if t' /= t 
       then fail (printTree e ++ " has type " ++ printTree t'
                  ++ ", expected: " ++ printTree t)
       else return e'

-- | Infers types of the expressionargument. 
inferExp :: EnvT -> Exp -> Err (Exp,Type)
inferExp env e = 
    case e of
      -- variable declaration and assignment
      EId x          -> do t <- lookupVarT env x
                           return (ETyped t e, t)
      EAss e1 e2     -> do (e1',t1) <- inferExp env e1
                           (e2',t2) <- inferExp env e2
                           if t1 == t2 
                                    then return (ETyped t1 (EAss e1' e2'), t1)
                                    else fail ("Assignment type mismatch: \n" ++
                                               "left-hand side " ++ printTree e1 ++ 
                                               " has type " ++ printTree t1 ++ 
                                               " but right-hand side " ++ printTree e2 ++ 
                                               " has type " ++ printTree t2)
                           
      -- literals                     
      EInt _         -> return (ETyped TInt e,  TInt)
      EDouble _      -> return (ETyped TDouble e, TDouble)
      ETrue          -> return (ETyped TBool ETrue, TBool)
      EFalse         -> return (ETyped TBool EFalse, TBool) 

      -- comparison expressions type-check similarly to ELtEq
      ENEq  e1 e2    -> do
              (ETyped TBool (ELtEq e1' e2'), TBool) <- inferExp env (ELtEq e1 e2)
              return (ETyped TBool (ENEq e1' e2'), TBool)                           
      EEq   e1 e2    -> do
              (ETyped TBool (ELtEq e1' e2'), TBool) <- inferExp env (ELtEq e1 e2)
              return (ETyped TBool (EEq e1' e2'), TBool)
      EGt   e1 e2    -> do
              (ETyped TBool (ELtEq e1' e2'), TBool) <- inferExp env (ELtEq e1 e2)
              return (ETyped TBool (EGt e1' e2'), TBool)           
      ELt   e1 e2    -> do
              (ETyped TBool (ELtEq e1' e2'), TBool) <- inferExp env (ELtEq e1 e2)
              return (ETyped TBool (ELt e1' e2'), TBool)                   
      EGtEq e1 e2    -> do
              (ETyped TBool (ELtEq e1' e2'), TBool) <- inferExp env (ELtEq e1 e2)
              return (ETyped TBool (EGtEq e1' e2'), TBool)           
      ---- 'base case'
      ELtEq e1 e2    -> do (e1',t1) <- inferExp env e1
                           (e2',t2) <- inferExp env e2
                           if t1 == t2 
                           then case t1 of
                                  TVoid -> fail "Comparison of void values" 
                                  _     -> return (ETyped TBool (ELtEq e1' e2'), TBool)
                           else fail ("Type mismatch: \n" ++
                                      printTree e1 ++ " has type " ++ 
                                      printTree t1 ++ " but " ++
                                      printTree e2 ++ " has type " ++
                                      printTree t2)
                           
      -- arithmetic expressions type-check similarly to EPlus
      EDiv   e1 e2   -> do (ETyped t (EPlus e1' e2'), t') <- inferExp env (EPlus e1 e2)
                           return (ETyped t (EDiv e1' e2'), t)
      ETimes e1 e2   -> do (ETyped t (EPlus e1' e2'), t') <- inferExp env (EPlus e1 e2)
                           return (ETyped t (ETimes e1' e2'), t)
      EMinus e1 e2   -> do (ETyped t (EPlus e1' e2'), t') <- inferExp env (EPlus e1 e2)
                           return (ETyped t (EMinus e1' e2'), t)
      ---- 'base case'
      EPlus  e1 e2   -> do (e1',t1) <- inferExp env e1
                           (e2',t2) <- inferExp env e2
                           if t1 == t2 
                           then case t1 of
                                  TBool -> fail "Arithmetic operation on bool values" 
                                  TVoid -> fail "Arithmetic operation on void values" 
                                  _     -> return (ETyped t1 (EPlus e1' e2'), t1)
                           else fail ("Type mismatch: \n" ++
                                      printTree e1 ++ " has type " ++ 
                                      printTree t1 ++ " but " ++
                                      printTree e2 ++ " has type " ++
                                      printTree t2)

      -- conjunction and disjunction
      EOr  e1 e2     -> do e1' <- checkExp env e1 TBool
                           e2' <- checkExp env e2 TBool
                           return (ETyped TBool (EOr e1' e2'), TBool)
      EAnd e1 e2     -> do e1' <- checkExp env e1 TBool
                           e2' <- checkExp env e2 TBool
                           return (ETyped TBool (EAnd e1' e2'), TBool)
      
      -- increments and decrements type-check similarly to EIncr
      EPDecr e       -> do (ETyped t (EIncr e'),t') <- inferExp env (EIncr e)
                           return (ETyped t (EPDecr e'), t)
      EPIncr e       -> do (ETyped t (EIncr e'),t') <- inferExp env (EIncr e)
                           return (ETyped t (EPIncr e'), t)
      EDecr  e       -> do (ETyped t (EIncr e'),t') <- inferExp env (EIncr e)
                           return (ETyped t (EDecr e'), t)
      EIncr  e       -> do (e',t) <- inferExp env e
                           case t of
                             TBool -> fail "Increment of boolean value"
                             TVoid -> fail "Increment of void value"
                             _     -> return (ETyped t (EIncr e'),t)
      -- function call
      EApp id exps   -> inferFun env e
      
      _              -> fail ("inferExp has a non exhaustive case pattern \n" 
                              ++ show e ++ " \n  " ++ printTree e) 


-- | Annotates return type of a function
-- and infers types of its argument expressions.
inferFun :: EnvT -> Exp -> Err (Exp,Type)
inferFun env (EApp id exps) = do (types, ftype)  <- lookupFunT env id
                                 if length exps == length types
                                    then do
                                       exps' <- zipWithM (checkExp env) exps types
                                       return (ETyped ftype (EApp id exps'),ftype)
                                    else
                                       fail "Incorrect no. arguments in function call"

                                 
-- inferFunHelper :: Env -> [Exp] -> [Type] -> Err Exp
-- inferFunHelper env e:[] t:[]         = return ()
-- inferFunHelper env exps types
-- inferFunHelper env (e:es) (t:ts) = do etyp <- inferExp env e
--                                       if etyp == t
--                                         then inferFunHelper env es ts
--                                         else fail "type error in argument of function call" 
-- inferFunHelper _ _ _             = fail "inferFunHelper has non exhaustive case pattern"                  

