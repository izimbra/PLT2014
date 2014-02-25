-- | This module defines data types and functions
-- used for constructing and manipulating
-- environments used in the different parts of compiler toolchain.

--module Environment where

module Environment (-- * Type Checking 
                    Env
                  , Context
                   -- * Interpretation
                  , IEnv
                  , IContext
                   -- * Compilation
                  , EnvC(..)
                  , Address
                  , Instruction
                  , emptyEnvC
                  , emit
                  , addVarC
                  , lookupVarC
                  , newBlockC
                  , exitBlockC
                  , TypeC
                  , typeToTypeC
                  , typeSize
                   -- * Other 
                  , Value(..) 
                  , boolToVal
                  , valToBool 
                  , argToType
                  , buildSig
                  , setVar
                  , setVars
                  , addVar
                  , addVars
                  , updateVar
                  , enterScope
                  , leaveScope
                  , addScope
                  , lookupVar
                  , lookupFun
                  , updateFun
                  , evalVar
                  , emptyEnv
                  ) where


import qualified Data.Map as M
import Control.Monad.State

import AbsCPP
import PrintCPP
import ErrM

-- | Variable context - map of variable ids to their types
type Context =  M.Map Id Type
type IContext = M.Map Id Value

-- | The environment of the type checker.
-- Includes symbol table for functions and list of variable contexts.
type Env =  (SigTab,  [Context]) -- mini version: [[(Id, Type)]]
type IEnv = (SigTabI, [IContext]) -- Interpreter version of corresponding tool

-- | Compilation environment.
data EnvC = E {  --temp commented out because it doesnt compile
 addresses   :: [[(Id,Address)]],
 nextLabel   :: Int,
 nextAddress :: Address,
 maxAddress  :: Address,
 stackSize   :: Int,
 maxSize     :: Int,
 code        :: [Instruction],
 funTable    :: SigTabC
}

-- | Jasmin assembly instruction 
type Instruction = String
-- | Variable storage address 
type Address = Int

-- | Constructs an empty compilation environment
emptyEnvC :: EnvC
emptyEnvC = E {
  addresses = [[]],
  nextLabel = 0,
  nextAddress = 1,
  maxAddress = 1,
  stackSize = 0,
  maxSize = 1,
  code = [],
  funTable = M.empty       
  }

-- | Emits a Jasmin assembly instruction.
emit :: Instruction -> State EnvC ()
emit i = modify (\env -> env{ code = i : code env })

-- | Adds a variable to current scope of variable storage.
addVarC :: Id -> Type -> State EnvC ()
addVarC x t = modify (\env -> env {
  addresses = case addresses env of (scope:rest) -> (((x,nextAddress env):scope):rest),
  nextAddress = nextAddress env + typeSize t
  })

-- | Looks up variable value in the variable storage.
lookupVarC :: Id -> State EnvC Address
lookupVarC x = do
  env <- get
  return $ look (addresses env) x 
 where
   look [] x = error $ "Unknown variable " ++ printTree x ++ "."
   look (scope:rest) x = case lookup x scope of
     Nothing -> look rest x
     Just a  -> a

-- | Creates a new scope in the variable storage.
newBlockC :: State EnvC Address
newBlockC = do
  modify (\env -> env {addresses = [] : addresses env})
  env <- get
  return $ nextAddress env

-- | Exits and remnoves the current scope of the variable storage.
exitBlockC :: Address -> State EnvC ()
exitBlockC a = modify (\env -> env {
   addresses = tail (addresses env),
   nextAddress = a
   })


-- | Symbol table for functions .
-- A map of function ids and their type signatures.
type SigTab  = M.Map Id Sig
type SigTabI = M.Map Id Def
-- | Compile-time function table, which uses JVM representations of
-- argument and return types.
type SigTabC = M.Map Id SigC -- or special SigC?

-- | Function type signature. Includes argument types and return type.
type Sig  = ([Type],  Type)
type SigC = ([TypeC], TypeC)

-- | Jasmin assembly represenation of basic Java types 
type TypeC = Char
-- | Converts 'Type' value to its Jasmin counterpart.
typeToTypeC :: Type -> TypeC
typeToTypeC TInt    = 'I'
typeToTypeC TDouble = 'D'
typeToTypeC TBool   = 'Z'
typeToTypeC TVoid   = 'V'

-- | Return size of the 'Type' value in bytes.
typeSize :: Type -> Int
typeSize t = case t of
  TInt    -> 1
  TDouble -> 2
  TBool   -> 1 -- booleans are represented by integers at runtime


data Value = VInt Integer | VDouble Double | VVoid | VUndef

instance Show Value where
  show (VInt i)    = show i
  show (VDouble d) = show d
  show VVoid       = "(void variable value)"
  show VUndef      = "(undefined variable value)"

-- | Converts regular 'Bool' value to 'Value' 
boolToVal :: Bool -> Value
boolToVal True  = VInt 1
boolToVal False = VInt 0

-- | Converts boolean 'Value' (implemented as @[ VInt 1 | VInt 0 ]@
-- to regular 'Bool'.
valToBool :: Value -> Bool
valToBool (VInt 1) = True
valToBool (VInt 0) = False

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
lookup_ [] x = fail $ "lookupVar: Unknown variable " ++ printTree x ++ ". Error, exiting program."
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

--Old definition
--addVar :: IEnv -> Id -> IEnv
--addVar (scope:rest) x = (((x,VUndef):scope):rest)

-- | Adds a new variable to the current variable scope,
-- or updates an existing variable    

addVars :: IEnv -> [Id] -> IEnv  --declaration of multiple variables
addVars ienv [] = ienv  --base case
addVars ienv (i:is) = addVars (addVar ienv i) is

addVar :: IEnv -> Id -> IEnv
addVar (funs, [] ) x          = (funs,[ (M.fromList [ (x, VUndef) ] ) ] )
--coming into an empty scope, creating the first IContext with a fromlist
--addVar (funs, [scope]   ) x = (funs, [(M.insert x VUndef scope)]) -- when coming into empty scope
addVar (funs, scope:rest) x = (funs, (M.insert x VUndef scope):rest)
addVar env id = error $ "pattern incomplete in addvar: \n" ++ show id ++ "\n" ++ show env
--updateVar :: Env -> Id -> Type -> Err Env
--updateVar (funs, scope:rest) x t = 
--    case M.lookup x scope of
--      Nothing -> return (funs, (M.insert x t scope):rest)
--      Just _  -> fail ("Variable " ++ printTree x ++ " already declared.")

setVars :: IEnv -> [Id] -> [Value] -> IEnv
setVars env [] _ = env  --base case. could also have base case for other list, but since they should have equal length or else it is an error of the caller, I didnt make that base case
setVars env (i:is) (v:vs) = setVars (setVar env i v) is vs --self recursive 


setVar :: IEnv -> Id -> Value -> IEnv
setVar (funs, []) x v = error $ "setVar: Unknown variable " ++ printTree x ++ ".Error, exiting program." --no scope at all : all scopes are empty
--setVar [] x _ = error $ "Unknown variable " ++ printTree x ++ "."
--setVar ([]:rest) x v = []:setVar rest x v -- current scope is empty list
setVar (funs, (scope:rest)) x v 
--      | M.null(scope) == True = setVar (funs, rest) x v -- I think this is wrong. It throws away the empty scope, which gives trouble in cases such as core005.cc. Just because the scope is empty doesn't mean you can throw it out, because the next leavescope will always throw out the top scope, and now all our values disappear. 
      | otherwise = case M.lookup x scope of--lookup: sucess -> set ,  fail -> go deeper
            Nothing -> (funs, (scope:rest')) 
                where (funs', rest') = setVar (funs, rest) x v
            Just _  -> (funs, (M.insert x v scope):rest)  
      
      
--      Nothing -> return (funs, (M.insert x t scope):rest)
      
--  = setVar (funs, rest) x v  
--if current scope empty but there are deeper scopes, jump down 1 level



--original
--setVar ((p@(y,_):scope):rest) x v 
--    | y == x = ((x,v):scope):rest
--    | otherwise = let scope':rest' = setVar (scope:rest) x v
--                   in (p:scope'):rest'

evalVar :: IEnv -> Id -> Value
evalVar (funs, []) x = error $ "evalVar: Unknown variable " ++ printTree x ++ ". Error, exiting program." --VUndef
evalVar (funs, (scope:rest)) x = --error ("evalVar of : " ++ show x) 
                         case M.lookup x scope of
                             Nothing -> evalVar (funs, rest)  x
                             Just v  -> v

-- type IEnv = (SigTabI, [IContext])
enterScope :: IEnv -> IEnv
enterScope (sig, conts) = (sig, (M.empty:conts))

leaveScope :: IEnv -> IEnv
leaveScope (sig, (c:cs)) = (sig, cs)


buildSig :: SigTabI -> [Def] -> IO SigTabI
buildSig i [] = return i  --base case
buildSig sig (d:ds) = buildSig (addFunSig sig d) ds

addFunSig :: SigTabI -> Def -> SigTabI
addFunSig sig (Fun t id a s) =  M.insert id (Fun t id a s) sig

-- | Extracts type from the function argument.
argToType :: Arg -> Type
argToType (Arg t _) = t
