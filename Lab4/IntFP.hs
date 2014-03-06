module Interpreter where

-- We need to use separate environments phi and gamma,
-- or use closures (one env. is enough in this case)

-- Closure can be specified in the grammar

import AbsFP
import PrintFP

data Funs = 
data Vars = Map Ident Value

data Value = VInt Int
           | VClosure Exp Env  -- or just closures


-- lookup
   -- overshadowing
                        
-- update


-- eval
   -- 1. build function table
                        
   -- 2. evaluate main
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
    
