module Interp where


import Control.Monad.Reader

import AbsFun
import ErrM


data Strategy = CallByName   -- evaluation strategy
              | CallByValue
data Env = Map Ident Exp     -- environment

data Value = VInt Int        -- values are integers 
           | VClos Exp Env   -- or closures

type Eval = ReaderT Env Err  -- evaluation monad: passes around state (Env)
                             -- and returns `Err a` (either result `a` or error)

interpret :: Strategy -> Program -> Eval Int
interpret _ (Prog defs) = undefined 
-- Add global function defs to env

-- Evaluate 'main'


eval :: Exp -> Eval Value
eval = undefined



