module Interpreter where

-- We need to use separate environments phi and gamma,
-- or use closures (one env. is enough in this case)

-- Closure can be specified in the grammar

import Data.Map

import AbsFP
import PrintFP

-- | Function symbol table
data Funs = Map Ident Exp

-- | Local variable storage
type Env = Map Ident Value

type Value = VInt Int
           | VClosure Exp Env  -- or just closures


-- lookup :: Ident -> Env -> Value
   -- overshadowing: function < variable < inner variable
                        
-- update :: Env -> Ident -> Value



-- interpret
   -- 1. build function table
funs :: [Def] -> Funs


fun1 :: Def ->  (String,Exp)
fun1 (Fun (Ident f) args exp) = do
    let re = reverse args
    let lambda = fun1helper re exp
    (f,lambda)
    


--fun1 :: Def ->  Exp
--fun1 (Fun id args exp) = do
--    let re = reverse args
--    fun1helper re exp
    
fun1helper :: [Ident] -> Exp -> Exp
fun1helper [] exp = exp
fun1helper (id:ids) exp = do
    let einner = EAbs id exp
    fun1helper ids einner
  
  

   -- 2. evaluate `main`

-- | Evaluate an expression
eval :: Exp -> Env -> Value
eval exp env =
  case exp of
    -- integer literals
    EInt i -> VInt i -- optional empty env.
    -- variables
    EVar
    
    EAdd
    ESub
    ELt
    EIf
    ELmb
    EApp
    -- call-by-name
    -- call-by-value
    
