module Interpreter where

-- We need to use separate environments phi and gamma,
-- or use closures (one env. is enough in this case)

-- Closure can be specified in the grammar

import Prelude hiding (lookup)
import qualified Data.Map as M

import AbsFP
import PrintFP

type Name = String

-- | Function symbol table
type Funs = M.Map Name Exp

-- | Local variable storage
type Vars = M.Map Ident Value


data Value = VInt Integer
           | VClosure Exp Vars  -- or just closures


interpret :: Program -> IO ()
interpret (Prog defs) = let funs = funTable defs
                            vars = M.empty
                            main = lookup "main" (funs,vars)
                        in  case main of
                              VClosure exp vars' -> let v = eval exp (funs,vars')
                                                    in  do case v of
                                                             VInt i -> putStrLn $ show i
                                                             _      -> error "Bad result"
                                                             
                              _                  -> error "Bad main function"

lookup :: Name -> (Funs,Vars) -> Value
lookup = undefined
   -- overshadowing: function < variable < inner variable
   -- error: not found                              
                        
-- update :: Env -> Ident -> Value



-- | Constructs function symbol table             
funTable :: [Def] -> Funs
funTable defs = let kas = map f2abs defs
                in M.fromList kas  
  where
    -- | Converts function definition to a lambda absraction
    f2abs :: Def -> (Name,Exp)
    f2abs (Fun (Ident f) args exp) = 
      let lambda = funhelper (reverse args) exp
      in (f,lambda)
 
    funhelper :: [Ident] -> Exp -> Exp
    funhelper [] exp = exp
    funhelper (id:ids) exp = 
      let einner = EAbs id exp
      in funhelper ids einner
  
   -- 2. evaluate `main`

-- | Evaluate an expression
eval :: Exp -> (Funs,Vars) -> Value
eval exp (funs,vars) =
  case exp of
    -- integer literals
    EInt i -> VInt i -- optional empty env.
    -- variables
    -- EVar
    
    -- EAdd
    -- ESub
    -- ELt
    -- EIf
    -- ELmb
    -- EApp
    -- call-by-name
    -- call-by-value
    
