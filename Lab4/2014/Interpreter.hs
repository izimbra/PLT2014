{-# LANGUAGE BangPatterns #-}

module Interpreter where

import Prelude hiding (lookup)
import Foreign.Marshal.Utils (fromBool)
import qualified Data.Map as M

import Debug.Trace
import AbsFP
import PrintFP

-- ECls Exp Vars
type Name = String 
type Funs = M.Map Name Exp
--type Vars = M.Map Name Exp
type Vars = M.Map Name Value

data Value = VInt Integer 
           | VClos Exp Vars
  deriving (Show)

-- Sets the call mode: call-by-name fro `True`,
-- call-by-value fro `False`
setCallMode :: Bool -> Def
setCallMode True  = Fun (Ident "callByName") [] (EInt 1) -- call-by-name
setCallMode False = Fun (Ident "callByName") [] (EInt 0) -- call-by-value

-- Looks up the special `callByName` in the argument function table
-- and returns call-by-name flag based on its value
callByName :: Funs -> Bool
callByName funs = case lookup "callByName" (funs,M.empty) of
  EInt 1 -> True
  _      -> False
  
-- Second parameter is call-by-name flag
interpret :: Program -> Bool -> IO ()
interpret (Prog defs) callMode = let funs   = funTable $ defs 
                                                         ++ [setCallMode callMode]
                                     vars   = M.empty
                                     main   = lookup "main" (funs,vars)
                                     result = eval main (funs,vars)
                        in do case result of
                                VInt i -> putStrLn $ show i
                                _      -> error $ "RUNTIME ERROR\n"
                                                  ++ "Bad result type:\n" ++ show result
                                                           

lookup :: Name -> (Funs,Vars) -> Exp
lookup id (funs,vars) =
  trace (show funs ++ "\n" ++show vars) $ case M.lookup id vars of
    Just v  -> case v of 
    			VInt i       -> EInt i
    			VClos e env  -> trace ("Closure:\n" ++ show env ++ show e) $ 
                                        lookup id (funs,env)

    Nothing -> case M.lookup id funs of
                 Just e  -> e
                 Nothing -> error $ "RUNTIME ERROR\n"
                                    ++ "unknown identifier " ++ id
            
   -- overshadowing: function < variable < inner variable
   -- error: not found                              
                        
-- update :: Env -> Ident -> Value
update :: Vars -> Name -> Value -> Vars
update env id v = M.insert id v env
-- M.union v1 v2 - v1 has priority

evalOperands :: Exp -> Exp -> (Funs,Vars) -> (Value,Value)
evalOperands e1 e2 (funs,vars) =
   let v1 = eval e1 (funs, vars)
       v2 = eval e2 (funs, vars)
   in (v1, v2)
--   in case (v1,v2) of
--        (EInt i1,EInt i2) -> (i1,i2)
--        _                 -> error $ "RUNTIME ERROR\n"
--                                      ++ "binary operation on non-integer expressions"
               
plus :: Value -> Value -> Value
plus (VInt n) (VInt m) = VInt $ n + m
plus _        _        = error "plus: type error"
   
-- Evaluate an expression
eval :: Exp -> (Funs,Vars) -> Value
eval exp (funs,vars) = --trace exp $
  case exp of
    -- integer literals
    EInt i -> VInt i -- base case -- optional empty env.
    -- binary operations
    EAdd e1 e2 -> plus (eval e1 (funs,vars)) (eval e2 (funs,vars)) -- = evalOperands e1 e2 (funs, vars)
      
    
    ESub e1 e2 -> let (VInt i1, VInt i2) = evalOperands e1 e2 (funs, vars)
                  in  VInt (i1-i2)
    ELt  e1 e2 -> let (VInt i1, VInt i2) = evalOperands e1 e2 (funs, vars)
                  in  VInt $ fromBool (i1<i2)
    -- function and argument look up, 
    EId (Ident id) -> eval (lookup id (funs,vars)) (funs, vars) -- EInt or ECls

    EIf cond e1 e2   -> case eval cond (funs, vars) of
                          VInt 1 -> eval e1 (funs,vars)
                          VInt 0 -> eval e2 (funs,vars)

    EAbs i e1       -> let cl = VClos (EAbs i e1) vars -- M.empty
                       in trace ("Closure: " ++ show cl) $ cl

    EApp e1 e2      -> case eval e1 (funs, vars) of
    					VClos (EAbs (Ident i) e') env -> 
    						let env' = update env i (eval e2 (funs,vars))
    						in trace ("Evaluating closure with :" ++
                                                          "\n vars: "++ show vars ++ 
                                                          "\n old env: " ++ show env ++ 
    						          "\n new env: " ++ show env') 
                                                    $ eval e' (funs,env') --add the "eval e2" to the closure environment, given the name of i

    					_ -> error $ "EApp first argument not closure: \n" ++ show e1
    _                -> error $ "Non-exhaustive case in eval: \n" ++ show exp

-- | Constructs function symbol table             
funTable :: [Def] -> Funs
funTable defs = let kas = map f2abs defs
                in M.fromList kas  
  where
    -- | Converts function definition to a lambda absraction
    f2abs :: Def -> (Name,Exp)
    f2abs (Fun (Ident f) args exp) = 
      let lambda = funhelper (reverse args) exp
      in  (f,lambda)
 
    funhelper :: [Ident] -> Exp -> Exp
    funhelper [] exp = exp
    funhelper (id:ids) exp = 
      let einner = EAbs id exp
      in funhelper ids einner
