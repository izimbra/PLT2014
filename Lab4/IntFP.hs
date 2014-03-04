module Interpreter where

-- We need to use separate environments phi and gamma,
-- or use closures (one env. is enough in this case)

-- Closure cand be specified in the grammar

import AbsFP
import PrintFP

data Value = VInt Int | VCl -- or just closures


