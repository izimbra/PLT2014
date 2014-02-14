module BuiltInFuncs where

import AbsCPP

argd = ADecl TDouble (Id "dummy")
argi = ADecl TInt    (Id "dummy")

printInt    :: Def
printInt    =  Func TVoid   (Id "printInt")    [argi] []

printDouble :: Def
printDouble =  Func TVoid   (Id "printDouble") [argd] []

readInt     :: Def
readInt     =  Func TInt    (Id "readInt")     [] []

readDouble  :: Def
readDouble  =  Func TDouble (Id "readDouble")  [] []

builtInFunctions = [printInt, printDouble, readInt, readDouble]



--data Def =
--   DFun Type Id [Arg] [Stm]

--newtype Id = Id String deriving (Eq,Ord,Show)

