module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Data.Map   --need to import this manually

type Env     = (Sig, [Context])      --functions and context stack -- book p72
type Sig     = Map Id ([Type], Type) --function type signature
type Context = Map Id Type       --variables with their types

--function definitions from p72
lookupVar :: Env -> Id -> Err Type
lookupVar env id = return Type_bool --dummy body for compiling. WRITE REAL

--lookupFun :: Env -> Id -> Err ([Type],Type) --and we should write the code for each
--updateVar :: Env -> Id -> Type -> Err Env
--updateFun :: Env -> Id -> ([Type],Type) -> Err Env
--newBlock  :: Env -> Env
--emptyEnv  :: Env
    

typecheck :: Program -> Err ()
typecheck p = return ()

inferExp :: Env -> Exp -> Err Type
inferExp env x = case x of
    ETrue   -> return Type_bool
    EInt n  -> return Type_int
    EId id  -> lookupVar env id
    EAdd exp1 exp2 ->
        inferBin [Type_int, Type_double] env exp1 exp2
        
inferBin :: [Type] -> Env -> Exp -> Exp -> Err Type
inferBin types env exp1 exp2 = do
    typ <- inferExp env exp1
    if elem typ types
        then
            checkExp env exp2 typ
        else
            fail $ "wrong type of expression " ++ printTree exp1
            
-- BNFC generated function printTree :: a -> String

--given p73
checkExp :: Env -> Type -> Exp -> Err ()
checkExp env typ exp = do
    typ2 <- inferExp env exp
    if (typ2 == typ) then
        return ()
      else
        fail $ "type of "   ++ printTree exp ++
               "expected "  ++ printTree typ ++
               "but found " ++ printTree typ2
               
checkStm :: Env -> Type -> Stm -> Err Env
checkStm env val x = case x of
    SExp exp -> do
        inferExp env exp
        return env
    SDecl typ x -> 
        updateVar env id typ
    SWhile exp stm -> do
        checkExp env Type_bool exp
        checkStm env val stm
        
checkStms :: Env -> [Stm] -> Err Env
checkStms env stms = case stms of
    [] -> return env
    x : rest -> do
        env' <- checkStm env x
        checkStms env' rest
