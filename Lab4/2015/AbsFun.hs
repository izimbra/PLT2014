

module AbsFun where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq,Ord,Show,Read)
data Program =
   Prog [Def]
  deriving (Eq,Ord,Show,Read)

data Def =
   DDef Ident [Ident] Exp
  deriving (Eq,Ord,Show,Read)

data Exp =
   EVar Ident
 | EInt Integer
 | EApp Exp Exp
 | EAdd Exp Exp
 | ESub Exp Exp
 | ELt Exp Exp
 | EIf Exp Exp Exp
 | EAbs Ident Exp
  deriving (Eq,Ord,Show,Read)

