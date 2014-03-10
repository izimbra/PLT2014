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
                              putStrLn $ "show main:"
                              putStrLn $ show main
                              putStrLn ""
                              putStrLn "Execution of main:"
                              putStrLn ""
                              case main of
                                VInt i -> evalClosure main (funs,vars) -- prints result
                                (VClosure exp _) -> let v = eval exp (funs,vars)
                                                    in evalClosure v (funs,vars)
  
evalClosure :: Value -> (Funs, Vars) ->  IO ()
evalClosure (VInt i) _ = putStrLn $ show i
                         
evalClosure (VClosure exp vars) (funs,vars') = let v = eval exp (funs, vars)
                                         in evalClosure v (funs, vars)
     --       _                  -> error "Bad main function"

lookup :: Name -> (Funs,Vars) -> Value
lookup id (funs,vars) =
  case M.lookup id vars of
    Just v  -> v --case v of
                 --VInt i -> EInt i
                 --VClosure exp vars -> 
    Nothing -> case M.lookup id funs of
                 Just exp -> VClosure exp M.empty
                 Nothing  -> error $ "Lookup failed. Looking for id:  " ++ show id ++ " funs:  " ++ show funs ++ " Vars : " ++  show vars
            
            
fromCls :: Value -> (Funs,Vars) -> Value
fromCls (VInt i) (f,v) = VInt i
fromCls (VClosure exp vars) (f,v) = eval exp (f,v) 
   -- overshadowing: function < variable < inner variable
   -- error: not found                              
-- | Evaluate an expression
eval :: Exp -> (Funs,Vars) -> Value
eval exp (funs,vars) =
  case exp of
    -- integer literals
    EInt i -> VInt i -- base case -- optional empty env.
    EAdd e1 e2 -> --error $ show $ eval e2 (funs,vars)
                  --error $ show exp 
                  let (VInt v1) = fromCls  (eval e1 (funs, vars)) (funs,vars)
                      (VInt v2) = fromCls  (eval e2 (funs, vars)) (funs,vars)
                  in  (VInt (v1+v2))
    ESub e1 e2 -> let (VInt v1) = eval e1 (funs, vars)
                      (VInt v2) = eval e2 (funs, vars)
                  in  (VInt (v1-v2))
    EId (Ident name) ->  lookup name (funs, vars) 
                      
    EAbs (Ident name) exp -> eval exp (funs, vars)
    EIf con tru fal -> case eval con (funs,vars) of
                        VInt 1 -> eval tru (funs,vars)
                        VInt 0 -> eval fal (funs,vars)
    ELt e1 e2 -> let (VInt v1) = fromCls ( eval e1 (funs,vars)) (funs,vars)
                     (VInt v2) = fromCls ( eval e2 (funs,vars)) (funs,vars)
                 in if (v1 < v2)
                    then VInt 1
                    else VInt 0
                                         
                        
    EApp e1 e2 -> let
--    _ ->    let
            v = eval e2 (funs, vars) --value of exp to input into f 
--            in error $ "Eval EApp: " ++ show exp ++ " v " ++ show v -- ++ " f " ++ show f      
            f = eval e1 (funs, vars)

                  in case f of
                    VInt i -> VInt i
                    VClosure (EAbs (Ident id) ex) vars' ->
                        let vars'' = M.insert id v vars'
                        in VClosure ex vars''
--                        in eval ex (funs, vars'') 
                    --VClosure ex vars' -> eval ex (funs, vars')
                    _ -> error $ "Eval EApp not exhaustive: " ++ show e1
    _ -> error $ "Eval not exhaustive: " ++ show exp
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
