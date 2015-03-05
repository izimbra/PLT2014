module Interpreter where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import Fun.Abs
import Fun.ErrM
import Fun.Print

-- * Entry point

-- | Program computes a number.

interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs) = do
  let cxt = Cxt (makeGlobal defs) emptyEnv
  v <- eval cxt (EVar $ Ident "main")
  case v of
    VInt i -> return i
    _      -> fail $ "main returned a function, but should return an integer"

  -- let cxt = Cxt strategy (makeSig prg) emptyEnv
  -- res <- eval (EVar $ Ident "main") `runReaderT` cxt
  -- case res of
  --   VInt i -> return i
  --   _      -> fail $ "main returned a function, but should return an integer"

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
  = VInt  Integer
  | VClos Exp Env

type Sig = Map Ident Exp
type Env = Map Ident Value

emptyEnv :: Env
emptyEnv = Map.empty

-- | Evaluation monad.

type Eval = ReaderT Cxt Err

-- | Evaluation function

eval :: Cxt -> Exp -> Err Value
eval cxt e = case e of
  EInt i   -> return $ VInt i
  EVar x   -> lookupCxt cxt x
  EAbs x b -> return $ VClos e $ cxtLocal cxt
  EApp f e -> do
    g <- eval cxt f
    v <- eval cxt e
    apply cxt g v
  EAdd e1 e2 -> do
    v1 <- eval cxt e1
    v2 <- eval cxt e2
    add v1 v2

apply :: Cxt -> Value -> Value -> Err Value
apply cxt f v = case f of
  VInt{} -> fail $ "attempt to apply an integer to an argument"
  VClos (EAbs x b) env -> do
    let env' = Map.insert x v env
    let cxt' = cxt { cxtLocal = env' }
    eval cxt' b

add :: Value -> Value -> Err Value
add (VInt i) (VInt j) = return $ VInt $ i + j
add _        _        = fail $ "attempt to add non-integers"

lookupCxt :: Cxt -> Ident -> Err Value
lookupCxt cxt@(Cxt glob loc) x = do
  case Map.lookup x loc of
    Just v -> return v
    Nothing -> case Map.lookup x glob of
      Just e  -> eval cxt e
      Nothing -> fail $ "unbound identifier " ++ printTree x

-- | Building the global environment
makeGlobal :: [Def] -> Sig
makeGlobal = foldr addToSig emptySig

emptySig :: Sig
emptySig = Map.empty

addToSig :: Def -> Sig -> Sig
addToSig (DDef f xs e) env = Map.insert f b env
  where b = foldr EAbs e xs
