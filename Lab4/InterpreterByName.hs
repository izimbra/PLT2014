module InterpreterByName where
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

interpretByName :: Program -> IO ()
interpretByName (Prog defs) = let 
                              funs = funTable defs
                              main = lookup "main" (funs,M.empty)
                              VClosure expMain _ = main
                              in do 
                                --putStrLn ""
                                --putStrLn $ show funs  
                                --putStrLn $ "show main: (callbyname)"
                                --putStrLn $ show main
                                --putStrLn ""
                                --putStrLn "Execution of main: (callbyname)"
                                --putStrLn ""
                                let result = ep (evalex expMain funs M.empty) funs M.empty
                                putStrLn $ show $ (\(VInt i) -> i) result
         
--self - recursive function meant to force evaluation as far as possible
ep = evalProgress
evalProgress :: Value -> Funs -> Vars -> Value
evalProgress (VInt i) f v = VInt i
evalProgress (VClosure exp vInner) f vOuter =
    let vars = M.union vInner vOuter
    in ep (evalex exp f vars) f vars         
           
--evaluate two expressions for use with binary operators. mostly to make the code cleaner and avoid copy-paste, since this can be used for the + - < operators                              
vBinEval :: Exp -> Exp -> Funs -> Vars -> (Value, Value)
vBinEval e1 e2 f v = --error $ show e1 ++ "   "  ++ show e2
    let  v1 = ep (evalex e1 f v) f v --force type checking for VInt only
         v2 = ep (evalex e2 f v) f v--,this should be complete evaluation
    in   (v1, v2)

--apply + or - operator to values                              
vBinArit :: (Value, Value) -> Oper -> Value
vBinArit (VInt i1, VInt i2) op  = VInt (i1 `op` i2)  -- add or sub
    
boolint :: Bool -> Integer
boolint True  = 1
boolint False = 0
      
--evaluate expressions with the global function list and sometimes in an environment with vars belonging to a VClosure                    
evalex :: Exp -> Funs -> Vars -> Value
evalex e f v = case e of
    EInt i -> VInt i
    EAdd e1 e2 -> vBinArit  (vBinEval e1 e2 f v) (+)    --they are completely independent, and must be evalable down to a number each        
    ESub e1 e2 -> vBinArit  (vBinEval e1 e2 f v) (-)
    ELt  e1 e2 -> let (VInt v1, VInt v2) = vBinEval e1 e2 f v
                  in  VInt (boolint ( v1 < v2))
    EId (Ident name) -> lookup name (f, v) --p130
    --EApp:the func will always be a lambda. we want to extract info from it
    EApp e1 e2 -> --error $ "\nDEBUG: \n E1: " ++ show e1 ++ " \n E2:" ++ show e2    
                case evalex e1 f v of
                    (VClosure (EAbs (Ident name) exp) vNew) ->  --error $ "ok \n" ++ show e1
                            let --arg = evalex e2 f v
                                arg = VClosure e2 v --does not evaluate before passing
                                vars' = M.union vNew v
                                vars'' = M.insert name arg vars'
                            in evalex exp f vars''
                    a -> a --error $ "not ok \n EXP:" ++ show e1 ++ "\nEVAL: " ++ show (evalex e1 f v)
                    --let (VClosure (EAbs (Ident name) exp) vNew) = evalex e1 f v  --it always returns a closure
                    
                  
    EIf test yes no -> let (VInt i) = ep ( evalex test f v) f v
                       in case i of
                         1 -> evalex yes f v
                         0 -> evalex no  f v                  
    EAbs (Ident name) exp -> VClosure e v --vars added to closure of lambda, book p 130                   
    _      -> error $ "\nevalex non exhaustive: cannot handle\n" ++ show e
      
lookup :: Name -> (Funs,Vars) -> Value
lookup id (funs,vars) =
  case M.lookup id vars of
    Just v  -> v 
    Nothing -> case M.lookup id funs of
                 Just exp -> VClosure exp M.empty
                 Nothing  -> error $ "\nLOOKUP FAILED. LOOKING FOR ID:  " ++ show id ++ "\n    IN FUNS:  " ++ show funs ++ "\n    AND VARS : " ++  show vars
                   
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
