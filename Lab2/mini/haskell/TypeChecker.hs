module TypeChecker where

import AbsMini
import PrintMini
import ErrM
import qualified Data.Map as Map

type Env = (Sig , [Context] )
type Sig = Map.Map Id ([Type] , Type)
type Context = Map.Map Id Type
--type Env = [[(Ident, Type)]]
--emptyEnv :: Env
--emptyEnv = [[]]

emptyEnv :: Env
emptyEnv =  (emptySig, [] )

emptySig :: Sig
emptySig =  Map.empty

def :: Def
def = DFun TInt (Id "hej") [] []
def2 :: Def
def2 = DFun TInt (Id "hej2") [] []
def3 :: Def
def3 = DFun TInt (Id "hej3") [] []

ds = [def, def2, def3]
s = emptySig

-- until here added from book p72 and self-made empty functions. 

typecheck :: Program -> Sig  -- first pass. get the signatures of functions by running through 
typecheck (Prog defs)  = getSignatures emptySig defs

getSignatures :: Sig -> [Def] -> Sig
getSignatures sig []        = sig  --base case returns finished sig
getSignatures sig (de:defs) = getSignatures sig' defs
    where
        sig' = getSign sig de
--getSignatures sig (de:defs) = do sig' <- getSign sig de
--                                 getSignatures sig' defs
                                 
getSign :: Sig -> Def -> Sig --get a Sig and update with given Def
getSign sig (DFun typ id args stms) = Map.insert id ([] , typ) sig


--typecheck :: Program -> Err ()
--typecheck (Prog stms) = checkStms emptyEnv stms

