module Interpreter where

-- We need to use separate environments phi and gamma,
-- or use closures (one env. is enough in this case)

-- Closure can be specified in the grammar

import Prelude hiding (lookup)
import qualified Data.Map as M

import AbsFP
import PrintFP

type Name = String

-- | Function symbol table
type Funs = M.Map Name Exp

-- | Local variable storage
type Vars = M.Map Name Exp --Value


--data Value = VInt Integer
--           | VClosure Exp Vars  -- or just closures


interpret :: Program -> IO ()
interpret (Prog defs) = let funs = funTable defs                            
                            vars = M.empty
                            main = lookup "main" (funs,vars)
                        in do putStrLn ""
                              putStrLn $ show funs  
                              putStrLn "Execution of main:"
                              let (EInt i) = eval main (funs,vars)
                              putStrLn $ show i
                              --putStrLn $ show (eval main (funs,vars))
--                              case main of
--                                VClosure exp vars' -> let v = eval exp (funs,vars')
--                                                    in  do case v of
--                                                             EInt i -> putStrLn $ show -i
--                                                             _      -> error "Bad result"
--                                                           
--                                _                  -> error "Bad main function"

lookup :: Name -> (Funs,Vars) -> Exp
lookup id (funs,vars) =
  case M.lookup id vars of
    Just v  -> v
    Nothing -> case M.lookup id funs of
                 Just exp -> exp --VClosure exp vars
                 Nothing  -> error "Lookup failed"
            
   -- overshadowing: function < variable < inner variable
   -- error: not found                              
                        
-- update :: Env -> Ident -> Value


  
   -- 2. evaluate `main`

-- | Evaluate an expression
eval :: Exp -> (Funs,Vars) -> Exp
eval exp (funs,vars) =
  case exp of
    -- integer literals
    EInt i -> exp -- base case -- optional empty env.
    EAdd e1 e2 -> let (EInt v1) = eval e1 (funs, vars)
                      (EInt v2) = eval e2 (funs, vars)
                  in  (EInt (v1+v2))
    ESub e1 e2 -> let (EInt v1) = eval e1 (funs, vars)
                      (EInt v2) = eval e2 (funs, vars)
                  in  (EInt (v1-v2))
--    EApp e1 e2 -> let f = eval e1
--                      a = eval e2
--                  in  case f of
--                        -- match on closure or ident---
--
--                        ECls Exp env -> ECls Exp update env
--                        _            -> error "Bad app"

    -- variables
    EId (Ident name) -> let exp = lookup name (funs,vars)
                        in exp                        
    --EId name -> let exp = lookup name funs
    --            in  exp --ECls exp M.empty
--                    -- construct empty env
    
    -- EAdd
    -- ESub
    -- ELt
    -- EIf
    -- EApp
    -- call-by-name
    -- call-by-value

--    ECls exp env -> case exp of
--                      EAbs -> undefined -- update env, shrink lambda
--                      _    -> eval exp

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
