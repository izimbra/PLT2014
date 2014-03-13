module Compiler where

import Control.Monad
import Control.Monad.State
import System.Environment (getArgs)
import System.Exit (exitFailure)

import AbsCPP
import LexCPP
import ParCPP
import PrintCPP
import ErrM

import Environment

-- a simple-minded compiler that doesn't need type annotations and works for integers only

compile :: String -> Program -> String
compile name p = unlines $ reverse $ code $ execState (compileProgram name p) emptyEnvC

compileProgram :: String -> Program -> State EnvC ()
compileProgram className (Prog defs) = do
  mapM_ emit [
    ".class public " ++ className,
    ".super java/lang/Object",
    "",
    ".method public <init>()V",
    "aload_0",
    "invokenonvirtual java/lang/Object/<init>()V",
    "return",
    ".end method",
    "",
    ".method public static main([Ljava/lang/String;)V",
    ".limit locals 100",   --- bogus limit
    ".limit stack 1000",   --- bogus limit

    -- calling the compiled 'main'
    "invokestatic " ++ className ++ "/main()I",
    "return",
    ".end method",
    ""
   ]
  mapM_ compileDef defs
--  emit "return"
--  emit ".end method"

-- | Compiles a function definition
compileDef :: Def -> State EnvC ()
compileDef (Fun t (Id f) args stms) = do
  -- method signature
  emit $ ".method public static " ++ f                      -- name
         ++ "(" ++ map (typeToTypeC . argType) args ++ ")" -- argument types
         ++ [typeToTypeC t]                                  -- return type   
         
         --    
  -- storage limits for local variables and stack
  emit $ ".limit locals 100"
  emit $ ".limit stack 100"

  mapM_ compileStm stms
  -- return 0 if function has no return statement
  -- for i = 1,...,m : addVarC(xi,ti)

  -- default return in case of no return statement
  case stms of
    [] -> defaultReturn t
--             emit $ ".end metod"
    _  -> case (last stms) of
            (SReturn e) -> emit $ ".end method" 
            _           -> defaultReturn t



-- | Generates default return code for a given function type.                             
defaultReturn :: Type ->  State EnvC ()
defaultReturn t =
  let insts = case t of
            TInt    -> ["iconst_0","ireturn"]
            TDouble -> ["dconst_0","dreturn"]
            TBool   -> ["iconst_0","ireturn"]
            TVoid   -> ["return"]
  in mapM_ emit $ insts ++ [".end method"]
  
  

-- | Concatenates two strings with a space between them. 
(+++) :: String -> String -> String
a +++ b = a ++ " " ++ b

--newLabel:: State EnvC ()   continue from here and then so SWhile below / emil 140226
--newLabel

  
  
compileStm :: Stm -> State EnvC ()
compileStm s = case s of
  SExp (EApp id es) -> do
    compileExp (EApp id es)
    --check the expression type and send pop or pop2 depending (book p103)
    --or if void, dont send anything at all
    --emit "pop"
    
  SExp (ETyped t e) -> do -- from Stm point of view, all Exp will be ETyped. This code is meant to behave as the rule on book p102
    compileExp e
    case t of
      TInt    -> emit "pop"
      TBool   -> emit "pop"
      TDouble -> emit "pop2"
      _       -> return ()
    
  
  SWhile e s -> do --book page 103
    test <- newLabelC
    end  <- newLabelC
    emit $ show test ++ ":"
    compileExp e
    emit $ "ifeq " ++ show end
    compileStm s
    emit $ "goto " ++ show test
    emit $ show end ++ ":"
    
    
    
  -- variable declaration, emits no code
  SDecl t x    -> addVarC x t
  -- variable assignment
  SAss x (ETyped t e) -> do  --following bok p102 for assignment statements
    compileExp e
    addr <- lookupVarC x
    case t of 
      TInt -> do
        emit "dup"
        emit $ "istore" ++ show addr
      TBool -> do
        emit "dup"
        emit $ "istore" ++ show addr
      TDouble -> do
        emit "dup2"
        emit $ "dstore" ++ show addr
      _       -> error $ "Compile error: Assign statement with type not (bool, int, double)"
      
  SAss x e     -> do
    compileExp e
    addr <- lookupVarC x
    emit ("istore " ++ show addr) 
  -- variable initialisation
  SInit t x e -> do
    addVarC x t
    compileExp e
    addr <- lookupVarC x
    emit ("istore " ++ show addr)
  
  SBlock stms  -> do
    a <- newBlockC
    mapM compileStm stms
    exitBlockC a
  -- SPrint e     -> do
  --   compileExp e
  --   emit $ "invokestatic Runtime/printInt(I)V"
  SReturn e    -> do
    compileExp e
    emit $ "ireturn"
  _            -> error $ "No match: " ++ show s
              --   return ()

--helper function for re-using code pattern
compileExpArithm :: Exp -> Exp -> Type -> String -> State EnvC ()
compileExpArithm e1 e2 t s = do
    compileExp e1
    compileExp e2
    emitTyped t s

compileExp :: Exp -> State EnvC ()

compileExp (ETyped t e) = case e of
    
  EInt i    -> emit ("bipush " ++ show i)
  EDouble d -> emit ("ldc2_w " ++ show d)
  ETrue     -> emit "bipush 1"
  EFalse    -> emit "bipush 0"
  
  EPlus  e1 e2 -> compileExpArithm e1 e2 t "add" --page 101 and 98
  EMinus e1 e2 -> compileExpArithm e1 e2 t "sub"
  ETimes e1 e2 -> compileExpArithm e1 e2 t "mul"
  EDiv   e1 e2 -> compileExpArithm e1 e2 t "div"

  EId x  -> do --corresponds to example with EVar
    a <- lookupVarC x
    --emit ("iload " ++ show a)
    emitTyped t ("load " ++ show a) 

  EApp (Id "printInt") [e] -> do --function call
--    mapM_ compileExp es
    compileExp e
    emit $ "invokestatic Runtime/printInt(I)V"
    
    
  


emitTyped :: Type -> Instruction -> State EnvC ()
emitTyped t i = emit (c ++ i) where
    c = case t of
        TInt -> "i"
        TDouble -> "d"
        TBool -> "i"
        _ -> error $ "emitTyped with type not (Int or Double or Bool)"

--something with the dup case that needs to consider when an expression of 
--any of these types leaves the value of the expression on the stack,
--so that the outer expr can use it to evaluate something. 
-- page 102 gives some instructions on this matter.
  
  
  
-- original EPlus below from given code
-- EPlus e1 e2 -> do --need to extend to handle the different combinations of int and double. the original given example only handles ints. 
--    compileExp e1
--    compileExp e2
--    emit "iadd"
--  ETyped _ e -> compileExp e



