module Interpreter where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

import AbsFun
import ErrM
import PrintFun


--- Data types

-- | Evaluation strategy.
data Strategy
  = CallByName
  | CallByValue

-- | Context for evaluation.
data Cxt = Cxt
  { cxtGlobal :: Sig
  , cxtLocal  :: Env
  , strategy :: Strategy
  }
type Sig = Map Ident Exp    -- ^ Signature: definitions of global functions.
type Env = Map Ident Value  -- ^ Environment: values of local identifiers.

data Value
  = VInt  Integer  -- ^ Numeric value.
  | VClos Exp Env  -- ^ Function closure.
  deriving Show

-- | Evaluation monad.
type Eval = ReaderT Cxt Err


--- Functions

-- | Entry point of interpreter.
--   A program computes a number.
interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs) = do
  let cxt = Cxt (makeGlobal defs) emptyEnv strategy
  v <- eval (EVar $ Ident "main") `runReaderT` cxt
  case v of
    VInt i -> return i
   -- VClos (EInt i) _ -> return i
    VClos exp env    -> do vv <- eval exp `runReaderT` cxt
                           case vv of
                             VInt k -> return k
                             _      -> fail $ "damn: "++show vv
    _      -> fail $ "main returned a function, but should return an integer\n"++ show v


-- | Create empty environment
emptyEnv :: Env
emptyEnv = Map.empty

-- | Looking up a local or global identifier (can fail).
lookupCxt :: Ident -> Eval Value
lookupCxt x = do
  cxt@(Cxt glob loc strat) <- ask
  case Map.lookup x loc of -- overshadowing
    Just v -> trace ("Local lookup "++(show x)++":"++show v) $ return v
    Nothing -> case Map.lookup x glob of
      Just e  -> trace ("Global lookup " ++ (show x) ++ ": " ++ show e) -- TODO remove trace
                 $  eval e
      Nothing -> fail $ "unbound identifier " ++ printTree x


-- | Evaluation function.  TODO: missing expressions, call-by-name.
eval :: Exp -> Eval Value
eval e = case e of
  EInt i   -> return $ VInt i
  EVar x   -> lookupCxt x
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
  EAbs _ _ -> trace ("\nEval lambda: "++show e) $ -- TODO remove
   do
    cxt <- ask
    return $ VClos e $ cxtLocal cxt
    -- OR:
    -- loc <- asks cxtLocal
    -- return $ VClos e loc
    -- OR:
    -- VClos e <$> asks cxtLocal
  EApp f e -> trace ("\nEval apply " ++ show f ++ " to " ++ show e) $ -- TODO remove
   do
    g   <- eval f
    cxt <- ask
    v   <- case strategy cxt of
      CallByValue -> eval e
      CallByName  -> return $ VClos e  -- creates closures even for integers! 
                                    emptyEnv -- or cxtLocal cxt?
    apply g v
  _ -> error ("non-exhaustive pattern in eval : " ++ show e  )
    

-- | Applying a function value to an argument value (can fail).
apply :: Value -> Value -> Eval Value
-- good1.fun: main =
-- (EApp 
--       (EApp 
--             (EApp (EVar (Ident "fst")) 
--                   (EApp (EVar (Ident "twice")) -- lookupCxt global -> closure w/em
--                         (EAbs (Ident "x") (EVar (Ident "x"))))) -> closure w/empty
--             (EInt 6))
--       (EInt 7))

-- leads to `apply
--   VClos (EApp (EVar (Ident "twice")) 
--               (EAbs (Ident "x") (EVar (Ident "x")))) (fromList [])
--   VClos (EInt 7) (fromList [])

apply f v = case f of
  VInt {} -> fail $ "attempt to apply an integer to an argument"
  --- CHANGE 1: added pattern for VClos
  ---    Feels strange to to have applications in closure, but fails otherwise
  VClos g@(EApp _ _) _  ->  trace ("\nApply EApp: "++ show f) $ do -- TODO remove 
    gcl <- eval g
    trace ("evals to: "++show gcl) $ apply gcl v
-- VClos 
--      (EAbs (Ident "x") (EApp (EVar (Ident "f")) 
--                              (EApp (EVar (Ident "f")) (EVar (Ident "x"))))) 
--      (fromList [(Ident "f",
--                  VClos (EAbs (Ident "x") (EVar (Ident "x"))) (fromList []))])
  VClos (EAbs x b) env -> do
    let env' = Map.insert x v env
    trace ("\tNew env: "++show env') $ -- TODO remove
    --z <-
     local (\ cxt -> cxt { cxtLocal = env' }) $ eval b
    --case z of
    --  VInt i  -> return z
    --  VClos exp en -> 
  _ -> error ("non-exhaustive pattern in apply : \n"++show f++"\n"++show v)



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
