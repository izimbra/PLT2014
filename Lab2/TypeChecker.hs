-- | Non-annotating type checker.
module TypeChecker where

--import qualified Data.Map as M
import AbsCPP
import PrintCPP
import ErrM
import BuiltIns
import Environment


typecheck :: Program -> Err () -- Program
typecheck (Prog defs) = do
            env <- buildFunTable emptyEnvT (defs ++ builtInFunctions)
            checkDefs env defs


-- | Builds a symbol table for functions
-- in a type-checking  environment.
buildFunTable :: EnvT -> [Def] -> Err EnvT -- or just SigTab
buildFunTable env []   = return env
buildFunTable env (d:ds) =
    case d of
      Fun ftype id args _ -> do env' <- updateFunT env id (map argType args, ftype)
                                buildFunTable env' ds                                 
      _                    -> fail "Bad function definition, buildFunTable"

checkDefs :: EnvT -> [Def] -> Err ()
checkDefs env []     = return ()
checkDefs env (d:ds) = do env' <- checkDef env d
                          checkDefs env' ds

checkDef :: EnvT -> Def -> Err EnvT 
checkDef env (Fun typ id args stms) = do env'  <- addArgsT (addScopeT env) args
                                         env'' <- updateVarT env' (Id "return") typ
--since return i a reserved word, there will never be a variable with that as id
--so we can use it to store the function type in every scope
                                         checkStms env'' stms


checkStms :: EnvT -> [Stm] -> Err EnvT
checkStms env [] = return env
checkStms env (st:stms) = do env' <- checkStm env st
                             checkStms env' stms

checkStm :: EnvT -> Stm -> Err EnvT
checkStm env s = 
    case s of
      SDecl t x         -> updateVarT env x t
      SAss x e          -> do t <- lookupVarT env x
                              checkExp env e t
                              return env      
      SInit typ id exp  -> do env' <- checkStm env (SDecl typ id)  --first declare
                              checkStm env' (SAss id exp)   --then assign

      SBlock stms       -> do checkStms (addScopeT env) stms
                              return env
      -- SPrint e          -> do inferExp env e
      --                         return env
      SDecls typ []     -> return env
      SDecls typ (i:is) -> do env' <- checkStm env (SDecl typ i)
                              checkStm env' (SDecls typ is)
      SReturn exp       -> do retType <- lookupVarT env (Id "return") 
                              checkExp env exp retType
                              return env
      SExp exp          -> do t <- inferExp env exp
                              return env --is there anything to actually do with an exp?
      SIfElse exp s1 s2 -> do checkExp env exp TBool
                              env'  <- checkStm env s1
                              env'' <- checkStm env s2
                              return env
      SWhile exp stm    -> do checkExp env exp TBool
                              env'  <- checkStm env stm
                              return env
                              
      --updateVars env ids typ
      _                 -> fail ( "case not exhaustive in checkstm  \n"
                                  ++ show s ++ " \n  " ++ printTree s) 

checkExp :: EnvT -> Exp -> Type -> Err () -- Err Exp
checkExp env e t = 
    do t' <- inferExp env e
       if t' /= t 
         then fail (printTree e ++ " has type " ++ printTree t'
                    ++ " expected " ++ printTree t)
         else return () -- ETyped r t
                        -- ETyped can be defined in the grammar
                        -- as internal rule ETyped Exp ::= "(" Type ")" Exp1
                        -- Parse will fail, but we will have the info in AST

inferExp :: EnvT -> Exp -> Err Type
inferExp env e = 
    case e of
      EId x          -> lookupVarT env x
      EInt _         -> return TInt
      EDouble _      -> return TDouble
      EApp id exps   -> inferFun env e
      ETrue          -> return TBool
      EFalse         -> return TBool

      --Assignment and type comparison, re-used many times 
      EAss e1 e2     -> do t1 <- inferExp env e1
                           t2 <- inferExp env e2
                           if t1 == t2 
                                    then return t1
                                    else fail (printTree e1 ++ " has type " ++ printTree t1
                                         ++ " but " ++ printTree e2 
                                         ++ " has type " ++ printTree t2)      
      --Comparisons
      ENEq  e1 e2    -> inferExp env (ELtEq e1 e2) --
      EEq   e1 e2    -> inferExp env (ELtEq e1 e2) -- This will allow comparison between voids, 
      EGt   e1 e2    -> inferExp env (ELtEq e1 e2) -- Which doesn't have an interpretation
      ELt   e1 e2    -> inferExp env (ELtEq e1 e2) 
      EGtEq e1 e2    -> inferExp env (ELtEq e1 e2)
      ELtEq e1 e2    -> do t0 <- inferExp env (EAss e1 e2)
                           return TBool
                           
      --Arithmetics
      EDiv   e1 e2   -> inferExp env (EPlus e1 e2) --
      ETimes e1 e2   -> inferExp env (EPlus e1 e2) --
      EMinus e1 e2   -> inferExp env (EPlus e1 e2) --
      EPlus  e1 e2   -> do t1 <- inferExp env (EAss e1 e2)
                           if t1 == TBool || t1 == TVoid
                             then fail "Arithmetic operation on bool or void type" 
                             else return t1

      --Logic And,Or
      EOr  e1 e2     -> inferExp env (EAnd e1 e2)
      EAnd e1 e2     -> do checkExp env e1 TBool
                           checkExp env e2 TBool
                           return TBool      
      --Incr group
      EPDecr exp     -> inferExp env (EIncr exp)
      EPIncr exp     -> inferExp env (EIncr exp)
      EDecr exp      -> inferExp env (EIncr exp)
      EIncr exp      -> do t <- inferExp env exp
                           if t == TBool || t == TVoid
                           then fail "Increment operation on bool/void"
                           else return t   
      --Catch-all, should never happen                          
      _               -> fail ("inferExp has a non exhaustive case pattern \n" 
                               ++ show e ++ " \n  " ++ printTree e) 



inferFun :: EnvT -> Exp -> Err Type
inferFun env (EApp id exps) = do (types, ftype)  <- lookupFunT env id
                                 inferFunHelper env exps types
                                 return ftype
                                 
                                
inferFunHelper :: EnvT -> [Exp] -> [Type] -> Err ()
inferFunHelper env [] []         = return ()
inferFunHelper env [] types      = fail "too few  args in function call"
inferFunHelper env exps []       = fail "too many args in function call"
inferFunHelper env (e:es) (t:ts) = do etyp <- inferExp env e
                                      if etyp == t
                                        then inferFunHelper env es ts
                                        else fail "type error in argument of function call" 
inferFunHelper _ _ _             = fail "inferFunHelper has non exhaustive case pattern"                  

