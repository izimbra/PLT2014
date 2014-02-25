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
compileProgram name (Prog defs) = do
  mapM_ emit [
    ".class public " ++ name,
    ".super java/lang/Object",
    "",
    ".method public <init>()V",
    "aload_0",
    "invokenonvirtual java/lang/Object/<init>()V",
    "return",
    ".end method",
    "",
    ".method public static main([Ljava/lang/String;)V",
    ".limit locals 100",  --- bogus limit
    ".limit stack 1000"   --- bogus limit
   ]
  mapM_ compileDef defs
  emit "return"
  emit ".end method"

-- | Compiles a function definition
compileDef :: Def -> State EnvC ()
compileDef (Fun t (Id f) args stms) = do
  -- method signature
  emit $ ".method public static " +++ f                      -- name
         ++ "(" ++ map (typeToTypeC . argToType) args ++ ")" -- argument types
         ++ [typeToTypeC t]                                  -- return type      


--   emit(.limit locals locals(f))
--   emit(.limit stack stack(f))

-- for i = 1,...,m : addVarC(xi,ti)
--  compile stms
--emit(.end method)


-- | Concatenates two strings with a space between them. 
(+++) :: String -> String -> String
a +++ b = a ++ " " ++ b


compileStm :: Stm -> State EnvC ()
compileStm s = case s of
  SDecl t x   -> addVarC x t
  SAss x e -> do
    compileExp e
    a <- lookupVarC x
    emit ("istore " ++ show a) 
  SBlock stms -> do
    a <- newBlockC
    mapM compileStm stms
    exitBlockC a
  SPrint e -> do
    compileExp e
    emit $ "invokestatic Runtime/printInt(I)V"

compileExp :: Exp -> State EnvC ()
compileExp e = case e of
  EId x  -> do
    a <- lookupVarC x
    emit ("iload " ++ show a)
  EInt i    -> emit ("bipush " ++ show i)
  EDouble d -> emit ("ldc2_w " ++ show d)
  
--can we do something like this, following book page 101?   -- it compiles, so I guess you can
  EPlus e1 e2 -> do
    compileExp e1
    compileExp e2
    case e1 of  --the type checker only allows int+int or double+double, so checking 1 of them is enough.
      (EInt x) -> emit "iadd"
      (EDouble x) -> emit "dadd"
      _ -> error $ "error: EPlus expression compiled with type not (Int or Double)" ++ show (EPlus e1 e2) --emit "" --should not happen. create error?
-------        
  EMinus e1 e2 -> do 
    compileExp e1
    compileExp e2
    case e1 of 
      (EInt x) -> emit "isub"
      (EDouble x) -> emit "dsub"
      _ -> error $ "error: EMinus expression compiled with type not (Int or Double)"++ show (EMinus e1 e2)



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



