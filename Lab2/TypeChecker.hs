module TypeChecker where

import qualified Data.Map as M
import AbsCPP
import PrintCPP
import ErrM
import BuiltInFuncs

-- We get Printer and ErrM monad from BNFC.
--
-- Type checker --
-- 2 passes
-- 1) build symb. table, should include built-in functions
-- 2) do type-checking and annotation
--
-- only 4 types needed (void is practical)
--
-- Program - list of defs. (functions only)
-- Should be rules for built-ins
--
-- Checking return statment is optional
-- When checking the func. def, we can carry the function type around during recursion)
--
-- We need to check that vars in arg.list are distinct
--
-- t x = e; can be viewed as synt. sugar, it can be replaceb by 2 statements:
-- 1) t x;
-- 2) x = e ;

-- check EApp f es 
--  (ts, t) <- lookUp f
--  smart way is to use zip:
--  let ets = zip es ts - but will truncate the longer list!
--  mapM (\(e,t) -> check e t) ets

-- | The environment of the type checker.
-- Includes symbol table for functions and list of variable contexts.
type Env = (SigTab, [Context]) -- mini version: [[(Id, Type)]]

-- | Symbol table for functions .
-- A map of function ids and their type signatures.
type SigTab = M.Map Id Sig

-- | Function type signature. Includes argument types and return type.
type Sig = ([Type], Type)

-- | Variable context - map of variable ids to their types
type Context =  M.Map Id Type

typecheck :: Program -> Err () -- Program
typecheck (Prog defs) = do
            env <- buildFunTable emptyEnv (defs ++ builtInFunctions)
            return ()
  --checkDefs emptyEnv defs

-- | Builds a symbol table for functions in the environment.
buildFunTable :: Env -> [Def] -> Err Env -- or just SigTab
buildFunTable env []   = return env
buildFunTable env (d:ds) =
    case d of
      Func ftype id args _ -> do env' <- updateFun env id (map argType args, ftype)
                                 buildFunTable env' ds                                 
      _                    -> fail "Bad function definition, buildFunTable"
    
                    
argType :: Arg -> Type
argType (ADecl atype _) = atype

checkDefs :: Env -> [Def] -> Err () -- Err Type?
checkDefs env []     = return ()
checkDefs env (d:ds) = do env' <- checkDef env d
                          checkDefs env' ds


--data Def =
--   Func Type Id [Arg] [Stm]

checkDef :: Env -> Def -> Err Env -- we only have 1 function definition
checkDef env d =
    case d of
      Func t i args stms -> do env' <- addArgs env args
                               checkStms env' stms 
                        
      --undefined --the args are added to the context
                                       --then go to statementss
      _               -> fail "Bad function definition, checkDef"


addArgs :: Env -> [Arg] -> Err Env
addArgs env [] = return env --base case 
addArgs env ( (ADecl typ id) :as) = do env' <- updateVar env id typ
                                       addArgs env' as

checkStms :: Env -> [Stm] -> Err Env
checkStms env [] = return env
checkStms env (st:stms) = do env' <- checkStm env st
                             checkStms env' stms

checkStm :: Env -> Stm -> Err Env
checkStm env s = 
    case s of
      SDecl t x       -> updateVar env x t
      SAss x e        -> do t <- lookupVar env x
                            checkExp env e t
                            return env      
      SBlock stms     -> do checkStms (addScope env) stms
                            return env
      SPrint e        -> do inferExp env e
                            return env

checkExp :: Env -> Exp -> Type -> Err () -- Err Exp
checkExp env e t = 
    do t' <- inferExp env e
       if t' /= t 
         then fail (printTree e ++ " has type " ++ printTree t'
                    ++ " expected " ++ printTree t)
         else return () -- ETyped r t
                        -- ETyped can be defined in the grammar
                        -- as internal rule ETyped Exp ::= "(" Type ")" Exp1
                        -- Parse will fail, but we will have the info in AST

inferExp :: Env -> Exp -> Err Type
inferExp env e = 
    case e of
      EVar x         -> lookupVar env x
      EInt _         -> return TInt
      EDouble _      -> return TDouble
      EAdd e1 e2     -> do t1 <- inferExp env e1
                           t2 <- inferExp env e2
                           if t1 == t2 
                             then return t1
                             else fail (printTree e1 ++ " has type " ++ printTree t1
                                         ++ " but " ++ printTree e2 
                                         ++ " has type " ++ printTree t2)



emptyEnv :: Env
emptyEnv = (M.empty, [])

-- | Adds a function signature to the environment,
-- or updates it if the function is already present.
updateFun :: Env -> Id -> Sig -> Err Env
updateFun (funs, scopes) id sig = let funs' = M.insert id sig funs
                                  in  return (funs', scopes)

-- | Looks up a function definition in the environment
lookupFun :: Env -> Id -> Err Sig
lookupFun (funs, _) f =
  case M.lookup f funs of
    Just sig -> return sig
    Nothing  -> fail ("Undefined function " ++ printTree f ++ ".")

-- | Adds a new variable to the current variable scope,
-- or updates an existing variable    
updateVar :: Env -> Id -> Type -> Err Env
updateVar (funs, scope:rest) x t = 
    case M.lookup x scope of
      Nothing -> return (funs, (M.insert x t scope):rest)
      Just _  -> fail ("Variable " ++ printTree x ++ " already declared.")

-- | Looks up a variable in the environment
lookupVar :: Env -> Id -> Err Type
lookupVar (_, scopes) = lookup_ scopes 
lookup_ [] x = fail $ "Unknown variable " ++ printTree x ++ "."
lookup_ (scope:rest) x = case M.lookup x scope of
                             Nothing -> lookup_ rest x
                             Just t  -> return t
addScope :: Env -> Env
addScope (funs, scopes) = (funs, M.empty:scopes)
