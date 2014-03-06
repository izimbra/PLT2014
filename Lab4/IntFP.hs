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
data Env = Map Ident Value

data Value = VInt Int
           | VClosure Exp Env  -- or just closures


-- lookup :: Ident -> Env -> Value
   -- overshadowing: function < variable < inner variable
                        
-- update :: Env -> Ident -> Value



-- interpret
   -- 1. build function table
funs :: [Def] -> Funs


  

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
    
