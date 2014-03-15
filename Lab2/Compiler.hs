module Compiler where

import Control.Monad
import Control.Monad.State
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Debug.Trace

import AbsCPP
import LexCPP
import ParCPP
import PrintCPP
import ErrM


import Environment

data IncrTiming = Pre | Post
-- a simple-minded compiler that doesn't need type annotations and works for integers only

compile :: String -> Program -> String
compile name p = unlines $ reverse $ code $ execState (compileProgram name p) emptyEnvC

compileProgram :: String -> Program -> State EnvC ()
compileProgram className_ (Prog defs) = do
  mapM_ emit [
    ".class public " ++ className_,
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
    "invokestatic " ++ className_ ++ "/main()I",
    "return",
    ".end method",
    ""
   ]
  modify (\env -> env {className = className_})  --adds className to state so it can be used in compile
  env <- get -- is this needed?

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
  --error "trikikki"
  trace ( "\nTRACE ARGS: " ++ show args ++"\nOF FUNCTION " ++ show f ++ "\nEND TRACEARGS" ) $ addArgsHelper args

  -- default return in case of no return statement
  case stms of
    [] -> defaultReturn t
--             emit $ ".end metod"
    _  -> case (last stms) of
            (SReturn e) -> emit $ ".end method" 
            _           -> defaultReturn t


addArgsHelper :: [Arg] -> State EnvC ()
addArgsHelper [] = do
    env <- get 
    trace ("\naddArgsHelper finished, env has: \n"++  show (addresses env)) $ emit ""
addArgsHelper ( (Arg aType id) : as) = trace ("\nAddArgsHelper: " ++ show (Arg aType id) ++ "\n" ++ show as ++ "\n") $ do
    addVarC id aType
    addArgsHelper as


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

compileStm :: Stm -> State EnvC ()
compileStm s = case s of
  --SExp (EApp id es) -> do probably old
  --  compileExp (EApp id es)
    --check the expression type and send pop or pop2 depending (book p103)
    --or if void, dont send anything at all
    --emit "pop"
    
  SExp (ETyped t e) -> do -- from Stm point of view, all Exp will be ETyped. This code is meant to behave as the rule on book p102
    compileExp (ETyped t e)
   -- case e of
   --     EAss _ _ -> return ()
   --     _ -> case t of
    case t of
        TInt    -> emit "pop"
        TBool   -> emit "pop"
        TDouble -> emit "pop2"
        _       -> return ()
    
  
  SWhile e s -> do --book page 103
    test <- newLabelC "TESTwhile"
    end  <- newLabelC "ENDwhile"
    emit $ test ++ ":"
    compileExp e
    emit $ "ifeq" +++ end
    compileStm s
    emit $ "goto" +++ test
    emit $ end ++ ":"

  SIfElse e s1 s2 -> do
    false <- newLabelC "FALSEif"
    true  <- newLabelC "TRUEif"
    compileExp e
    emit $ "ifeq" +++ false
    compileStm s1
    emit $ "goto" +++ true
    emit $ false ++ ":"
    compileStm s2
    emit $ true ++ ":"
    
  -- variable declaration, emits no code
  SDecl t x    -> addVarC x t

  SDecls t (x:[]) -> compileStm (SDecl t x)
  SDecls t (x:xs) -> do
    compileStm (SDecl t x)
    compileStm (SDecls t xs)
  -- variable assignment
  -- SAss x (ETyped t e) -> trace ("TRACE\n" ++ show (ETyped t e )++"\nEndTrace\n") $ do  --following bok p102 for assignment statements
  --   compileExp (ETyped t e) --- DO NOT UNWRAP
  --   addr <- lookupVarC x
  --   case t of 
  --     TInt -> do
  --       emit "dup"
  --       emit $ "istore" +++ show addr
  --     TBool -> do
  --       emit "dup"
  --       emit $ "istore" +++ show addr
  --     TDouble -> do
  --       emit "dup2"
  --       emit $ "dstore" +++ show addr
  --     _       -> error $ "Compile error: Assign statement with type not (bool, int, double)"
      
  --SAss x e     -> trace (show e) $ do 
  --  compileExp e
  --  addr <- lookupVarC x
  --  emit ("istore " ++ show addr) 
  -- variable initialisation
  SInit t x e -> --trace ("\nTRACE:\n " ++ show t ++ "\n " ++ show x ++ "\n " ++ show e ++ "\nEND TRACE\n")  $ 
   do
    compileStm (SDecl t x)
    --compileStm (SAss x e) --we removed this from the grammar and decided to use only SExp EAss. The call becomes ugly tho =)
    
    compileStm (SExp (ETyped t (EAss (ETyped t (EId x)) e)))  --perHaps you can just compileExp and skip the Stm, I wasn't sure.
    --compileExp (ETyped t (EAss (ETyped t (EId x)) e)) --seems bad, gets wrong stack height
    --addVarC x t
    --compileExp e
    --addr <- lookupVarC x
    --emit ("istore " ++ show addr)
  
  SBlock stms  -> do
    a <- newBlockC
    mapM compileStm stms
    exitBlockC a
  -- SPrint e     -> do
  --   compileExp e
  --   emit $ "invokestatic Runtime/printInt(I)V"
  SReturn (ETyped t e) -> do
    compileExp (ETyped t e)
    emit $ case t of
            TInt    -> "ireturn"
            TDouble -> "dreturn"
            TBool   -> "ireturn"
            --TVoid   -> error $ "SReturn TVoid not handled"
            TVoid   -> "return"
  _            -> error $ "No match in compileExp: " ++ show s
              --   return ()

--helper function for re-using code pattern
compileExpArithm :: Exp -> Exp -> Type -> String -> State EnvC ()
compileExpArithm e1 e2 t s = do
    compileExp e1
    compileExp e2
    emitTyped t s


argTypeC :: Type -> Char 
argTypeC TInt    = 'I'
argTypeC TDouble = 'D'
argTypeC TBool   = 'I' --maybe should be Z. and maybe this function isn't needed because the environment.hs has something similar
argTypeC TVoid   = 'V'
argTypeC t       = error $ "bad type sent to argTypeC: " ++ show t

funCallHelper :: Exp -> String -> State EnvC () --The String here is the list of chars for ArgTypes used in the jvm such as (II)

funCallHelper (ETyped fType (EApp (Id name) [])) s = do --base case, no more args to compile
    env <- get
    let className_ = (className env)
    emit $ "invokestatic " ++ className_ ++ "/" ++ name ++ "(" ++ s ++ ")" ++ [argTypeC fType]

funCallHelper (ETyped fType (EApp (Id name) ( (ETyped aType arg):args))) s = do --self recursive
    compileExp (ETyped aType arg)
    let s' = s ++ [argTypeC aType]
    funCallHelper (ETyped fType (EApp (Id name) ( args))) s'  --1 arg popped and 1 argTypeC added


invokeRuntime :: String -> [Exp] -> State EnvC ()
invokeRuntime s []  = emit $ "invokestatic Runtime/" ++ s
invokeRuntime s [e] = do
    compileExp e
    invokeRuntime s []

compileExp :: Exp -> State EnvC ()

compileExp (ETyped t e) = --trace ("\nTRACE COMPILEEXP ETYPED: \n" ++ show e ++"\nEnd trace\n" ) $ 
 case e of
  -- Built-in functions
  EApp (Id "printInt")    [e] -> invokeRuntime "printInt(I)V"    [e]
  EApp (Id "printDouble") [e] -> invokeRuntime "printDouble(D)V" [e]
  EApp (Id "readInt")     _   -> invokeRuntime "readInt()I"      []
  EApp (Id "readDouble")  _   -> invokeRuntime "readDouble()D"   []

  EApp (Id name ) args -> funCallHelper (ETyped t (EApp (Id name) args)) "" --general function call
   
  EInt i    -> emit ("bipush " ++ show i)
  EDouble d -> emit ("ldc2_w " ++ show d)
  ETrue     -> emit "bipush 1"
  EFalse    -> emit "bipush 0"
  
  EPlus  e1 e2 -> compileExpArithm e1 e2 t "add" --page 101 and 98
  EMinus e1 e2 -> compileExpArithm e1 e2 t "sub"
  ETimes e1 e2 -> compileExpArithm e1 e2 t "mul"
  EDiv   e1 e2 -> compileExpArithm e1 e2 t "div"

    --http://cs.au.dk/~mis/dOvs/jvmspec/ref-Java.html
  ELt   e1 e2 -> compileExpCompare "if_icmplt"  e1 e2
  EGt   e1 e2 -> compileExpCompare "if_icmpgt"  e1 e2
  ELtEq e1 e2 -> compileExpCompare "if_icmple"  e1 e2
  EGtEq e1 e2 -> compileExpCompare "if_icmpge"  e1 e2
  EEq   e1 e2 -> compileExpCompare "if_icmpeq"  e1 e2
  ENEq  e1 e2 -> compileExpCompare "if_icmpne"  e1 e2

  EIncr  e -> compileIncr e "iadd" Pre
  EDecr  e -> compileIncr e "isub" Pre
  EPIncr e -> compileIncr e "iadd" Post
  EPDecr e -> compileIncr e "isub" Post

--  EIncr  e -> compilePreIncDec  e "iadd"
--  EDecr  e -> compilePreIncDec  e "isub"
--  EPIncr e -> compilePostIncDec e "iadd"
--  EPDecr e -> compilePostIncDec e "isub"
  -- variable reference loads its value on stack
  EId x  -> do --corresponds to example with EVar
    a <- lookupVarC x
    emitTyped t ("load " ++ show a)  --emit ("iload " ++ show a)

  -- variable assignment
  EAss (ETyped t (EId x)) e -> do -- explicit match on EId because we don't want
    addr <- lookupVarC x               -- to load 'x', just look up its address
    --trace ("TRACE\n" ++ show (ETyped t e )++"\nEndTrace\n") $ do  --following bok p102 for assignment statements
    compileExp e
    case t of 
      TInt -> do
        emit "dup"
        emit $ "istore" +++ show addr
      TBool -> do
        emit "dup"
        emit $ "istore" +++ show addr
      TDouble -> do
        emit "dup2"
        emit $ "dstore" +++ show addr
      _       -> error $ "COMPILATION ERROR\n" ++
                         "Assigment of type not in [bool, int, double]" -- should be caught in type checker?

--do
--    compileExp  e
--    emit "bipush 1"
--    emit "iadd"
--    let (ETyped t (EId x)) = e
--    a <- lookupVarC x 
--    emit $ "istore" +++ show a

 -- EDecr e -> do
 --   compileExp e
 --   emit "bipush 1"
 --   emit "isub"

    --compileExp (ETyped TInt (EPlus (ETyped TInt e)  (ETyped TInt (EInt 1))))
    
  _ -> error ( "\n\nERROR NON EXHAUSTIVE COMPIlEEXP \n " ++
               show (ETyped t e) ++ 
               "cannot compile case of \n" ++ show e)
 
compileExp e = error $ "NON TYPED EXP IN COMPILEEXP \n" ++ (show e)

--generalised compiler for pre- and post- increment and decrements
compileIncr :: Exp -> String -> IncrTiming -> State EnvC ()  
compileIncr e operation timing = do
    compileExp e
    emit $ case timing of 
        Pre -> ""
        Post -> "dup"
    
    emit "bipush 1"
    emit operation

    emit $ case timing of
        Pre -> "dup"
        Post -> ""

    let (ETyped t (EId x)) = e
    a <- lookupVarC x 
    emit $ "istore" +++ show a

--compilePostIncDec :: Exp -> String -> State EnvC ()
--compilePostIncDec e operation = do
--    compileExp e
--    emit "dup" --dup before operation, so that the original value is left on the stack to return
--    emit "bipush 1"
--    emit operation   
--    let (ETyped t (EId x)) = e
--    a <- lookupVarC x 
--    emit $ "istore" +++ show a-
--
--compilePreIncDec :: Exp -> String -> State EnvC ()
--compilePreIncDec e operation = do
--    compileExp  e
--    emit "bipush 1"
--    emit operation
--    emit $ "dup" --we need a dup here since we want the expression to have a return value . in the case of SExp, SExp will pop -one extra time to take care of it. 
--    let (ETyped t (EId x)) = e
--    a <- lookupVarC x 
--    emit $ "istore" +++ show a

--the jvm Operator is passed as the  string argument
--book page 104 on how to compile ELt . Same pattern for all 6 which have their own JVM operator.
compileExpCompare :: String -> Exp -> Exp -> State EnvC () 
compileExpCompare jvmOp e1 e2 = do
    let label = drop 7 jvmOp -- all these 6 ops have the same length 9 and we want the last 2

    true <- newLabelC ("TRUE"++label)
    emit "bipush 1"
    compileExp e1
    compileExp e2
    emit $ jvmOp ++ " " ++ true
    emit "pop"
    emit "bipush 0"
    emit $ true ++ ":"

    


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



