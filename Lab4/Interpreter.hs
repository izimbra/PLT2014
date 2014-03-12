module Interpreter where

-- We need to use separate environments phi and gamma,
-- or use closures (one env. is enough in this case)

-- Closure can be specified in the grammar

import Prelude hiding (lookup)
import Foreign.Marshal.Utils (fromBool)
import qualified Data.Map as M

import AbsFP
import PrintFP



-- data Value = VInt Integer
---            | VClosure Exp Vars  -- or just closures


interpret :: Program -> IO ()
interpret (Prog defs) = let funs          = funTable defs                            
                            vars          = M.empty
                            (ECls main _) = lookup "main" (funs,vars)
                            result        = eval main (funs,vars)
                        in do putStrLn ""
                              putStrLn $ show funs  
                              putStrLn ""
--                              case main of
--                                VClosure exp vars' -> let v = eval exp (funs,vars')
                              case result of
                                EInt i -> putStrLn $ show i
                                _      -> error $ "Bad result: " ++ show result
                                                           
--                                _                  -> error "Bad main function"

lookup :: Name -> (Funs,Vars) -> Exp
lookup id (funs,vars) =
  case M.lookup id vars of
    Just e  -> e
    Nothing -> case M.lookup id funs of
                 Just e  -> ECls e vars
                 Nothing -> error $ "Lookup failed: " ++ id ++ " not found"
            
   -- overshadowing: function < variable < inner variable
   -- error: not found                              
                        
-- update :: Env -> Ident -> Value
update :: Vars -> Name -> Exp -> Vars
update env id v = M.insert id v env
-- M.union v1 v2 - v1 has priority

evalOperands :: Exp -> Exp -> (Funs,Vars) -> (Integer,Integer)
evalOperands e1 e2 (funs,vars) =
   let (EInt i1) = eval e1 (funs, vars)
       (EInt i2) = eval e2 (funs, vars)
   in  (i1,i2)
                  
-- | Evaluate an expression
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
    
    EApp e1 e2 -> let f = eval e1 (funs, vars) -- ECls
                      a = eval e2 (funs, vars) -- EInt 
                  in  case f of
                        -- match on closure or ident

                        ECls (EAbs (Ident id) e) env -> ECls e (update env id a) --e update env
                        _            -> error $ "Bad app: " ++ show f

    -- variables
    EId (Ident id) -> lookup id (funs,vars) -- EInt or ECls
                                    -- construct empty env
    



    -- EIf

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
