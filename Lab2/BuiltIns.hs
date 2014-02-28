module BuiltIns where

import AbsCPP

argd = Arg TDouble (Id "dummy")
argi = Arg TInt    (Id "dummy")

printInt    :: Def
printInt    =  Fun TVoid   (Id "printInt")    [argi] []

printDouble :: Def
printDouble =  Fun TVoid   (Id "printDouble") [argd] []

readInt     :: Def
readInt     =  Fun TInt    (Id "readInt")     [] []

readDouble  :: Def
readDouble  =  Fun TDouble (Id "readDouble")  [] []

builtInFunctions = [printInt, printDouble, readInt, readDouble]



--data Def =
--   DFun Type Id [Arg] [Stm]

--newtype Id = Id String deriving (Eq,Ord,Show)

