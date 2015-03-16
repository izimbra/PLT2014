module Interpreter2 where

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
--   A program computes a number.

interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs) = do
  let cxt = Cxt (makeGlobal defs) emptyEnv strategy
  v <- eval (EVar $ Ident "main") `runReaderT` cxt
  case v of
    VInt i -> return i
    _      -> fail $ "main returned a function, but should return an integer"

-- | Evaluation strategy.

data Strategy
  = CallByName
  | CallByValue

-- | Context for evaluation.

data Cxt = Cxt
  { cxtGlobal :: Sig
  , cxtLocal  :: Env
  , strategy  :: Strategy                 
  }

data Value
  = VInt  Integer  -- ^ Numeric value.
  | VClos Exp Env  -- ^ Function closure.

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
  EVar x   -> do v <- lookupCxt x
                 case v of
                   VClos e' env -> local (\ cxt -> cxt { cxtLocal = env }) 
                                   $ eval e' -- see sides 7, p.41
                   VInt _       -> return v
  EAdd e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    add v1 v2
  ESub e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    sub v1 v2
  ELt e1 e2 -> do
    (VInt i1) <- eval e1
    (VInt i2) <- eval e2
    return $ VInt $ if i1 < i2 then 1 else 0
  EIf cond e1 e2 -> do 
    (VInt c) <- eval cond
    if c == 1 then eval e1 else eval e2
  EAbs x b -> do
    cxt <- ask
    return $ VClos e $ cxtLocal cxt -- see slides 7, p.44
    -- OR:
    -- loc <- asks cxtLocal
    -- return $ VClos e loc
    -- OR:
    -- VClos e <$> asks cxtLocal
  EApp f a -> do
    g <- eval f
    stgy <- asks strategy
    loc  <- asks cxtLocal
    v <- case stgy of 
           CallByValue -> eval a
           CallByName  -> return $ VClos a loc
    apply g v

-- | Applying a function value to an argument value (can fail).

apply :: Value -> Value -> Eval Value
apply f v = case f of
  VInt{} -> fail $ "attempt to apply an integer to an argument"
  VClos (EAbs x b) env -> do
    let env' = Map.insert x v env -- overshadowing
    local (\ cxt -> cxt { cxtLocal = env' }) $ eval b

-- | Looking up a local or global identifier (can fail).

lookupCxt :: Ident -> Eval Value
lookupCxt x = do
  cxt@(Cxt glob loc strat) <- ask
  case Map.lookup x loc of
    Just v -> return v
    Nothing -> case Map.lookup x glob of
      Just e  -> eval e
      Nothing -> fail $ "unbound identifier " ++ printTree x

-- | Numeric addition of two values (can fail).

add :: Value -> Value -> Eval Value
add (VInt i) (VInt j) = return $ VInt $ i + j
add _        _        = fail $ "attempt to add non-integers"

-- | Numeric subtraction

sub :: Value -> Value -> Eval Value
sub (VInt i) (VInt j) = return $ VInt $ i - j
sub _        _        = fail $ "attemtp to subtract non-integers"
 
-- | Building the global environment.

makeGlobal :: [Def] -> Sig
makeGlobal = foldr addToSig emptySig

emptySig :: Sig
emptySig = Map.empty

addToSig :: Def -> Sig -> Sig
addToSig (DDef f xs e) env = Map.insert f b env
  where b = foldr EAbs e xs
