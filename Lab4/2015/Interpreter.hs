module Interpreter where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import AbsFun
import ErrM
import PrintFun

-- | Entry point of interpreter.
--
--   A program computes a number.

interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs) = do
  let s'  = case strategy of
              CallByValue -> (VInt 0)
              CallByName  -> (VInt 1)
      env' = Map.insert (Ident "callbyname") s' emptyEnv
      cxt = Cxt (makeGlobal defs) env'
  v <- eval (EVar $ Ident "main") `runReaderT` cxt
  case v of
    VInt i -> return i
    VClos (EInt i) _ -> return i   
    _      -> fail $ "main returned a function, but should return an integer\n"-- ++ show v

-- | Evaluation strategy.

data Strategy
  = CallByName
  | CallByValue

-- | Context for evaluation.

data Cxt = Cxt
  { cxtGlobal :: Sig
  , cxtLocal  :: Env
  }

data Value
  = VInt  Integer  -- ^ Numeric value.
  | VClos Exp Env  -- ^ Function closure.
  deriving Show

type Sig = Map Ident Exp    -- ^ Signature: definitions of global functions.
type Env = Map Ident Value  -- ^ Environment: values of local identifiers.

emptyEnv :: Env
emptyEnv = Map.empty

-- | Evaluation monad.

type Eval = ReaderT Cxt Err

-- | Evaluation function.  TODO: missing expressions, call-by-name.

eval :: Exp -> Eval Value
eval e = case e of
  EInt i   -> return $ VInt i
  EVar x   -> lookupCxt x
  EAbs x b -> do
    cxt <- ask
    return $ VClos e $ cxtLocal cxt
    -- OR:
    -- loc <- asks cxtLocal
    -- return $ VClos e loc
    -- OR:
    -- VClos e <$> asks cxtLocal
  EApp f e -> do
    g <- eval f
    (VInt strat) <- lookupCxt (Ident "callbyname")
    v <- case strat of
      0 -> eval e
      1 -> return $ VClos e  emptyEnv --cxtLocal cxt
    apply g v
  EAdd e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    add v1 v2
  ESub e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    sub v1 v2
    
  EIf cond e1 e2 -> do 
    (VInt c) <- eval cond
    if c == 1 then eval e1 else eval e2
  ELt e1 e2 -> do
    (VInt i1) <- eval e1
    (VInt i2) <- eval e2
    return $ if i1 < i2 then (VInt 1) else (VInt 0)
  _ -> error ("non-exhaustive pattern in eval : " ++ show e  )
    

-- | Applying a function value to an argument value (can fail).

apply :: Value -> Value -> Eval Value
apply f v = case f of
  VInt{} -> fail $ "attempt to apply an integer to an argument"
  VClos (EAbs x b) env -> do
    let env' = Map.insert x v env
    local (\ cxt -> cxt { cxtLocal = env' }) $ eval b

 -- _ -> error ("non-exhaustive pattern in apply : \n"++show f ++ "\n" ++ show v  )
-- | Looking up a local or global identifier (can fail).

lookupCxt :: Ident -> Eval Value
lookupCxt x = do
  cxt@(Cxt glob loc) <- ask
  case Map.lookup x loc of
    Just v -> return v
    Nothing -> case Map.lookup x glob of
      Just e  -> eval e
      Nothing -> fail $ "unbound identifier " ++ printTree x

-- | Numeric addition of two values (can fail).

add :: Value -> Value -> Eval Value
add (VInt i) (VInt j) = return $ VInt $ i + j
add _        _        = fail $ "attempt to add non-integers"

sub :: Value -> Value -> Eval Value
sub (VInt i) (VInt j) = return $ VInt $ i - j
sub _        _        = fail $ "attempt to sub non-integers"

-- | Building the global environment.

makeGlobal :: [Def] -> Sig
makeGlobal = foldr addToSig emptySig

emptySig :: Sig
emptySig = Map.empty

addToSig :: Def -> Sig -> Sig
addToSig (DDef f xs e) env = Map.insert f b env
  where b = foldr EAbs e xs
