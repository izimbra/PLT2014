module BuiltInFuncs where

import AbsMini

argd = ADecl TDouble (Id "dummy")
argi = ADecl TInt    (Id "dummy")

printInt    :: Def
printInt    =  DFun TVoid   (Id "printInt")    [argi] []

printDouble :: Def
printDouble =  DFun TVoid   (Id "printDouble") [argd] []

readInt     :: Def
readInt     =  DFun TInt    (Id "readInt")     [] []

readDouble  :: Def
readDouble  =  DFun TDouble (Id "readDouble")  [] []

builtInFunctions = [printInt, printDouble, readInt, readDouble]



--data Def =
--   DFun Type Id [Arg] [Stm]

--newtype Id = Id String deriving (Eq,Ord,Show)

