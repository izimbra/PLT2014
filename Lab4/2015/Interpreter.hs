{-# LANGUAGE BangPatterns #-}

module Interpreter where

import Prelude hiding (lookup)
import Foreign.Marshal.Utils (fromBool)
import qualified Data.Map as M

import AbsFP
import PrintFP

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
interpret (Prog defs) callMode = let funs   = funTable $ defs ++ [setCallMode callMode]
                                     vars   = M.empty
                                     main   = lookup "main" (funs,vars)
                                     result = eval main (funs,vars)
                        in do case result of
                                EInt i -> putStrLn $ show i
                                _      -> error $ "RUNTIME ERROR\n"
                                                  ++ "Bad result type:\n" ++ show result
                                                           

lookup :: Name -> (Funs,Vars) -> Exp
lookup id (funs,vars) =
  case M.lookup id vars of
    Just e  -> e
    Nothing -> case M.lookup id funs of
                 Just e  -> e
                 Nothing -> error $ "RUNTIME ERROR\n"
                                    ++ "unknown identifier " ++ id
            
   -- overshadowing: function < variable < inner variable
   -- error: not found                              
                        
-- update :: Env -> Ident -> Value
update :: Vars -> Name -> Exp -> Vars
update env id v = M.insert id v env
-- M.union v1 v2 - v1 has priority

evalOperands :: Exp -> Exp -> (Funs,Vars) -> (Integer,Integer)
evalOperands e1 e2 (funs,vars) =
   let v1 = eval e1 (funs, vars)
       v2 = eval e2 (funs, vars)
   in case (v1,v2) of
         (EInt i1,EInt i2) -> (i1,i2)
         _                 -> error $ "RUNTIME ERROR\n"
                                      ++ "binary operation on non-integer expressions"
                  
-- Evaluate an expression
eval :: Exp -> (Funs,Vars) -> Exp
eval exp (funs,vars) =
  case exp of
    -- integer literals
    EInt i -> exp -- base case -- optional empty env.
    -- binary operations
    EAdd e1 e2 -> let (i1,i2) = evalOperands e1 e2 (funs, vars)
                  in  EInt (i1+i2)
    ESub e1 e2 -> let (i1,i2) = evalOperands e1 e2 (funs, vars)
                  in  EInt (i1-i2)
    ELt  e1 e2 -> let (i1,i2) = evalOperands e1 e2 (funs, vars)
                  in  EInt $ fromBool (i1<i2)
    -- function and argument look up, 
    EId (Ident id) -> eval (lookup id (funs,vars)) (funs, vars) -- EInt or ECls

    EApp e1 e2 -> let f = eval e1 (funs, vars) -- ECls
                      a = eval e2 (funs, vars) -- EInt
                      --This is where we need to force evaluation 
                      --to use call by value. We tried the pragma Bang 
                      --suggested in the google group, we also tried the
                      --seq, the ErrM, and some cases, but all of them 
                      --still didn't force evaluation so as to make 
                      --good2.fun loop endlessly. 
                      --Only with trace (show vars) does it loop
                      --as expected, so that apparently forces evaluation
                      --of Grow in good2. But that is not a useful final
                      --solution.
                      --if (callByName funs)
                      --then eval e2 (funs, vars) -- EInt 
                      --else (eval e2 (funs, vars)) -- EInt 
                  in  case f of
                        -- match on closure or ident
                        ECls (EAbs (Ident id) e) env ->
                             let env' = env -- M.union env vars
                             in  eval e (funs,(update env' id a))
                                                            -- update overshadows global ids 

--                        _       -> eval (EApp f a) (funs,vars)
                        _  -> error $ "Bad app: \n" ++ show f ++ "\n" ++ show a 

    EAbs _ _       -> ECls exp vars
    ECls e env     -> eval e (funs, env)
    EIf cond e1 e2   -> case eval cond (funs, vars) of
                          EInt 1 -> eval e1 (funs,vars)
                          EInt 0 -> eval e2 (funs,vars)
--    _                -> error $ "Non-exhaustive case in eval: \n" ++ show exp
    -- call-by-name
    -- call-by-value

    -- ECls exp env -> case exp of
    --                   EAbs -> undefined -- update env, shrink lambda
    --                   _    -> eval exp



-- | Constructs function symbol table             
funTable :: [Def] -> Funs
funTable defs = let kas = map f2abs defs
                in M.fromList kas  
  where
    -- | Converts function definition to a lambda absraction
    f2abs :: Def -> (Name,Exp)
    f2abs (Fun (Ident f) args exp) = 
      let lambda = funhelper (reverse args) exp
      in (f,lambda)
 
    funhelper :: [Ident] -> Exp -> Exp
    funhelper [] exp = exp
    funhelper (id:ids) exp = 
      let einner = EAbs id exp
      in funhelper ids einner
