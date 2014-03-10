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
type Vars = M.Map Name Value


data Value = VInt Integer
           | VClosure Exp Vars  -- or just closures
  deriving (Eq,Ord,Show)

interpret :: Program -> IO ()
interpret (Prog defs) = let funs = funTable defs                            
                            vars = M.empty
                            main = lookup "main" (funs,vars)
                        in do putStrLn ""
                              putStrLn $ show funs  
                              putStrLn "Execution of main:"
                              case main of 
                                VInt i -> putStrLn $ show i
                                VClosure expmain _ -> 
                                    putStrLn $ show $ 
                                    eval expmain (funs,vars) 
                              --let v = eval main (funs,vars)
                              --putStrLn $ show v
                              --putStrLn $ show (eval main (funs,vars))
--                              case main of
--                                VClosure exp vars' -> let v = eval exp (funs,vars')
--                                                    in  do case v of
--                                                             EInt i -> putStrLn $ show -i
--                                                             _      -> error "Bad result"
--                                                           
--                                _                  -> error "Bad main function"

lookup :: Name -> (Funs,Vars) -> Value
lookup id (funs,vars) =
  case M.lookup id vars of
    Just v  -> v --case v of
                 --VInt i -> EInt i
                 --VClosure exp vars -> 
    Nothing -> case M.lookup id funs of
                 Just exp -> VClosure exp M.empty
                 Nothing  -> error "Lookup failed"
            
   -- overshadowing: function < variable < inner variable
   -- error: not found                              
                        
-- update :: Env -> Ident -> Value



   -- 2. evaluate `main`

-- | Evaluate an expression
eval :: Exp -> (Funs,Vars) -> Value
eval exp (funs,vars) =
  case exp of
    -- integer literals
    EInt i -> VInt i -- base case -- optional empty env.
    EAdd e1 e2 -> let (VInt v1) = eval e1 (funs, vars)
                      (VInt v2) = eval e2 (funs, vars)
                  in  (VInt (v1+v2))
    ESub e1 e2 -> let (VInt v1) = eval e1 (funs, vars)
                      (VInt v2) = eval e2 (funs, vars)
                  in  (VInt (v1-v2))
    EId (Ident name) -> let v = lookup name (funs, vars)
                        in case v of 
                           VInt i -> v
                           VClosure ex _ -> eval ex (funs,vars)
    _          -> error $ show exp
    
--    EApp e1 e2 -> undefined --case e1 of
                    --EId (Ident name) -> let e1' = eval e1 (funs,vars)
                    --                    in eval (EApp e1' e2) (funs,vars)
                    --EAbs (Ident var) lmb -> undefined
--                    error $ show e1
                  --let f = eval e1
                  --    a = eval e2
                  
--                  in  case f of
--                        -- match on closure or ident---
--
--                        ECls Exp env -> ECls Exp update env
--                        _            -> error "Bad app"

    -- variables
   -- EId (Ident name) -> let exp = lookup name (funs,vars)
   --                     in eval exp (funs,vars)
    --EApp e1 e2       -> error $ show e2                        
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
