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


compile :: String -> Program -> String
compile name p = unlines $ reverse $ code $ execState (compileProgram name p) emptyEnvC

compileProgram :: String -> Program -> State EnvC ()
compileProgram className_ (Prog defs) = do
  modify (\env -> env {className = className_})
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
  mapM_ compileDef defs


-- | Compiles a function definition
compileDef :: Def -> State EnvC ()
compileDef (Fun t (Id f) args stms) = do
  -- method signature
  emit $ ".method public static " ++ f                     -- name
         ++ "(" ++ map (typeToTypeC . argType) args ++ ")" -- argument types
         ++ [typeToTypeC t]                                -- return type   
         
         --    
  -- storage limits for local variables and stack
  emit $ ".limit locals 100"
  emit $ ".limit stack 100"
  modify (\env -> env { addresses = [], nextAddress = 0 })

  -- add arguments to enviroment
  addArgsHelper args

  mapM_ compileStm stms
  
  -- default return in case of no return statement
  case stms of
    [] -> defaultReturn t
    _  -> case (last stms) of
            (SReturn e) -> emit $ ".end method" 
            _           -> defaultReturn t

  emit ""
    
  where -- Add arguments to enviroment
        addArgsHelper :: [Arg] -> State EnvC ()
        addArgsHelper [] = do return ()
        addArgsHelper ( (Arg aType id) : as) = do
          addVarC id aType
          addArgsHelper as

        -- Generate default return code for a given function type.                             
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
    
  SExp (ETyped t e) -> do
    compileExp (ETyped t e)
    popTyped t -- pop the previously duplicated value if it's not used 

  -- variable declaration, emits no code
  SDecl t x    -> addVarC x t

  SDecls t (x:[]) -> compileStm (SDecl t x)
  SDecls t (x:xs) -> do
    compileStm (SDecl t x)
    compileStm (SDecls t xs)

  -- variable initialisation
  SInit t x e     -> do
    compileStm (SDecl t x)
    compileStm (SExp (ETyped t (EAss (ETyped t (EId x)) e)))


  SWhile e s -> do
    test <- newLabelC "while_TEST"
    end  <- newLabelC "while_END"
    emit $ test ++ ":"
    compileExp e
    emit $ "ifeq" +++ end
    compileStm s
    emit $ "goto" +++ test
    emit $ end ++ ":"

  SIfElse e s1 s2 -> do
    false <- newLabelC "if_FALSE"
    true  <- newLabelC "if_TRUE"
    compileExp e
    emit $ "ifeq" +++ false
    compileStm s1
    emit $ "goto" +++ true
    emit $ false ++ ":"
    compileStm s2
    emit $ true ++ ":"
      
  SBlock stms  -> do
    a <- newBlockC
    mapM compileStm stms
    exitBlockC a

  SReturn (ETyped t e) -> do
    compileExp (ETyped t e)
    emit $ case t of
            TInt    -> "ireturn"
            TDouble -> "dreturn"
            TBool   -> "ireturn"
            TVoid   -> "return"
  _            -> error $ "COMPILATION ERROR\n"
                          ++ "No match in compileExp: " ++ show s


funCallHelper :: Exp -> String -> State EnvC () --The String here is the list of chars for ArgTypes used in the jvm such as (II)

funCallHelper (ETyped fType (EApp (Id name) [])) s = do --base case, no more args to compile
    env <- get
    let className_ = (className env)
    emit $ "invokestatic " ++ className_ ++ "/" ++ name ++ "(" ++ s ++ ")" ++ [typeToTypeC fType]

funCallHelper (ETyped fType (EApp (Id name) ( (ETyped aType arg):args))) s = do --self recursive
    compileExp (ETyped aType arg)
    let s' = s ++ [typeToTypeC aType]
    funCallHelper (ETyped fType (EApp (Id name) ( args))) s'  --1 arg popped and 1 argTypeC added

-- Invoke a function in Runtime class
invokeRuntime :: String -> [Exp] -> State EnvC ()
invokeRuntime s []  = emit $ "invokestatic Runtime/" ++ s
invokeRuntime s [e] = do
    compileExp e
    invokeRuntime s []

 
-- | Compile expression.
compileExp :: Exp -> State EnvC ()
compileExp (ETyped t e) = --trace ("\nTRACE COMPILEEXP ETYPED: \n" ++ show e ++"\nEnd trace\n" ) $ 
 case e of
  -- Built-in functions
  EApp (Id "printInt")    [e] -> invokeRuntime "printInt(I)V"    [e]
  EApp (Id "printDouble") [e] -> invokeRuntime "printDouble(D)V" [e]
  EApp (Id "readInt")     _   -> invokeRuntime "readInt()I"      []
  EApp (Id "readDouble")  _   -> invokeRuntime "readDouble()D"   []

  EApp (Id name ) args -> funCallHelper (ETyped t (EApp (Id name) args)) "" --general function call
   
  EInt i    -> if i > 200 
               then emit ("sipush " ++ show i)
               else emit ("bipush " ++ show i) 
  EDouble d -> emit ("ldc2_w " ++ show d)
  ETrue     -> emit "iconst_1"
  EFalse    -> emit "iconst_0"
  
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

  EIncr  e -> compileIncr e t "add" Pre --it isunwrapped here so we send an untyped Exp and its type separately. 
  EDecr  e -> compileIncr e t "sub" Pre --the function accepts that and it works. it may not follow convention
  EPIncr e -> compileIncr e t "add" Post --but I think it's worth it. the alternative would be to pass the whole
  EPDecr e -> compileIncr e t "sub" Post --ETyped EPIncr again, which becomes ugly and loses the point of separation done here.

  -- variable reference loads its value on stack
  EId x  -> do --corresponds to example with EVar
    a <- lookupVarC x
    emitTyped t ("load " ++ show a)  --emit ("iload " ++ show a)

  -- variable assignment--following bok p102 for assignment statements
  EAss (ETyped t (EId x)) e -> do -- explicit match on EId because we don't want
    addr <- lookupVarC x          -- to load 'x', just look up its address   
    compileExp e
    dupTyped t
    emitTyped t ("store" +++ show addr)

  EAnd e1 e2 -> do
    false <- newLabelC "and_FALSE"
    end   <- newLabelC "and_END"
    compileExp e1
    emit $ "ifeq" +++ false -- false if 1st exp evals to 0
    compileExp e2
    emit $ "ifeq" +++ false
    emit "iconst_1"         -- if we arrive here, both exp are non-zero
    emit $ "goto" +++ end
    emit $ false ++ ":"
    emit "iconst_0"         -- add EAnd result 0 
    --emit $ "goto" +++ end
    emit $ end ++ ":"       -- either 1 or 0 on stack
    
  EOr  e1 e2 -> do --error $ "EOr not implemented yet in compileExp"
    true  <- newLabelC "TRUEor"
    let truejump = "ifne" +++ true  -- http://cs.au.dk/~mis/dOvs/jvmspec/ref-ifne.html
    end <- newLabelC ("ENDor")
    compileExp e1 --e1 on stack
    emit truejump --stack empty
    compileExp e2 --e2 on stack
    emit truejump --stack empty
    emit "iconst_0" --0 on stack . if we arrive here, neither was true so we are false
    emit $ "goto" +++ end
    emit $ true ++":"       --label
    emit "iconst_1" --1 on stack. if we arrive here, either expression was true
    emit $ end ++ ":" --0 or 1 on stack
    
  _ -> error ( "COMPILATION ERROR\n" ++
               show (ETyped t e) ++ 
               "\n +++ ++ ++++ cannot compile case of \n" ++ show e)

 where -- Helper function for re-using pattern for compiling
       -- arithmetic expressions (add, sub, mul, div).  
       compileExpArithm :: Exp -> Exp -> Type -> String -> State EnvC ()
       compileExpArithm e1 e2 t s = do
           compileExp e1
           compileExp e2
           emitTyped t s

compileExp e = error $ "NON TYPED EXP IN COMPILEEXP \n" ++ (show e)


-- Emit type-specific dup instruction.
dupTyped :: Type -> State EnvC ()
dupTyped t = emit $ case t of
                      TDouble -> "dup2"
                      _       -> "dup"
                      
-- Emit type-specific pop instruction.
popTyped :: Type -> State EnvC ()
popTyped t = case t of
               TInt    -> emit "pop"
               TBool   -> emit "pop"
               TDouble -> emit "pop2"
               _       -> return ()



-- Generalised compiler for pre- and post- increment and decrements
compileIncr :: Exp -> Type -> Instruction -> IncrTiming -> State EnvC ()  
compileIncr e t i timing = do

    compileExp e
    emit $ case timing of 
        Pre -> ""
        Post -> dup
    
    emit one        
    emitTyped t i  -- add or sub according to type 

    emit $ case timing of
        Pre -> dup
        Post -> ""

    let (ETyped t (EId x)) = e
    a <- lookupVarC x 
    emitTyped t ("store" +++ show a)
    where (dup,one) = case t of TInt    -> ("dup" , "iconst_1")
                                TDouble -> ("dup2", "dconst_1") 

-- the JVM operator is passed as the  string argument
-- book page 104 on how to compile ELt . Same pattern for all 6 which have their own JVM operator.
compileExpCompare :: String -> Exp -> Exp -> State EnvC () 
compileExpCompare jvmOp e1 e2 = do
    let label = drop 7 jvmOp -- all these 6 ops have the same length 9 and we want the last 2

    true <- newLabelC ("TRUE"++label)
    emit "iconst_1"
    compileExp e1
    compileExp e2
    emit $ jvmOp ++ " " ++ true
    emit "pop"
    emit "iconst_0"
    emit $ true ++ ":"

-- Helper function for emitting type-specific versions of instructions.
-- Used for loads, stores and arithmetic instructions. 
emitTyped :: Type -> Instruction -> State EnvC ()
emitTyped t i = emit (c ++ i) where
    c = case t of
        TInt    -> "i"
        TDouble -> "d"
        TBool   -> "i"
        _ -> error $ "emitTyped with type not (Int or Double or Bool)"

