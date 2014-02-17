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

