module Environment where

import qualified Data.Map as M
import AbsCPP
import PrintCPP
import ErrM

-- | Variable context - map of variable ids to their types
type Context =  M.Map Id Type
type IContext = M.Map Id Value

-- | The environment of the type checker.
-- Includes symbol table for functions and list of variable contexts.
type Env = (SigTab, [Context]) -- mini version: [[(Id, Type)]]
type IEnv = (SigTab, [IContext])

-- | Symbol table for functions .
-- A map of function ids and their type signatures.
type SigTab = M.Map Id Sig

-- | Function type signature. Includes argument types and return type.
type Sig = ([Type], Type)


data Value = VInt Integer | VDouble Double | VUndef

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


emptyEnv :: Env
emptyEnv = (M.empty, [])

-- | Adds a function signature to the environment,
-- or updates it if the function is already present.
updateFun :: Env -> Id -> Sig -> Err Env
updateFun (funs, scopes) id sig = let funs' = M.insert id sig funs
                                  in  return (funs', scopes)


--Interpreter functions

addVar :: IEnv -> Id -> IEnv
addVar (scope:rest) x = (((x,VUndef):scope):rest)

setVar :: IEnv -> Id -> Value -> IEnv
setVar [] x _ = error $ "Unknown variable " ++ printTree x ++ "."
setVar ([]:rest) x v = []:setVar rest x v
setVar ((p@(y,_):scope):rest) x v 
    | y == x = ((x,v):scope):rest
    | otherwise = let scope':rest' = setVar (scope:rest) x v
                   in (p:scope'):rest'

evalVar :: IEnv -> Id -> Value
evalVar [] x = error $ "Unknown variable " ++ printTree x ++ "."
evalVar (scope:rest) x = case lookup x scope of
                             Nothing -> evalVar rest x
                             Just v  -> v
enterScope :: IEnv -> IEnv
enterScope env = []:env

leaveScope :: IEnv -> IEnv
leaveScope (_:env) = env
