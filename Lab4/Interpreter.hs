module Interpreter where
import Prelude hiding (lookup)
import qualified Data.Map as M
import AbsFP
import PrintFP
type Name = String
type Funs = M.Map Name Exp
type Vars = M.Map Name Value --only for closures

data Value = VInt Integer
           | VClosure Exp Vars
  deriving (Eq,Ord,Show)

type Oper = (Integer -> Integer -> Integer)

interpret :: Program -> IO ()
interpret (Prog defs) = let funs = funTable defs
                            main = lookup "main" (funs,M.empty)
                            VClosure expMain _ = main
                        in do putStrLn ""
                              putStrLn $ show funs  
                              putStrLn $ "show main:"
                              putStrLn $ show main
                              putStrLn ""
                              putStrLn "Execution of main:"
                              putStrLn ""
                              let round1 = evalex expMain funs M.empty
                              putStrLn $ show round1
                              --case round1 of
                              --  VInt i -> return
                              --  VClosure 
                              --putStrLn $ show (evalex expMain funs)
                              --putStrLn $ show ( eval main (funs, M.empty))
                              
vBinEval :: Exp -> Exp -> Funs -> Vars -> (Value, Value)
vBinEval e1 e2 f v = 
    let (VInt v1) = evalex e1 f v --force type checking for VInt only
        (VInt v2) = evalex e2 f v --,this should be complete evaluation
    in  ((VInt v1),(VInt v2))
                              
vBinArit :: (Value, Value) -> Oper -> Value
vBinArit (VInt i1, VInt i2) op  = VInt (i1 `op` i2)  -- add or sub
    
boolint :: Bool -> Integer
boolint True  = 1
boolint False = 0
                          
evalex :: Exp -> Funs -> Vars -> Value
evalex e f v = case e of
    EInt i -> VInt i
    EAdd e1 e2 -> vBinArit  (vBinEval e1 e2 f v) (+)    --they are completely independent, and must be evalable down to a number each        
    ESub e1 e2 -> vBinArit  (vBinEval e1 e2 f v) (-)
    ELt  e1 e2 -> let (VInt v1, VInt v2) = vBinEval e1 e2 f v
                  in  VInt (boolint ( v1 < v2))
    EId (Ident name) -> case lookup name (f, M.empty) of
                            VInt i -> VInt i
                            VClosure exp vars -> evalex exp f v 
                        
    _      -> error $ "evalex non exhaustive: " ++ show e
      
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
fromCls (VClosure exp vars) (f,v) = fromCls (eval exp (f,v)) (f,v) 
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
                      
    EAbs (Ident name) ex -> VClosure exp M.empty
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
                    --VClosure ex vars' -> 
                       
                    --eval ex (funs, vars')
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
