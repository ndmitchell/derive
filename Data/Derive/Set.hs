
module Data.Derive.Set(makeSet) where

import Language.Haskell.TH.All
import Data.Char
import Data.List

{-
data Computer = Laptop {weight :: Int, memory :: Int}
              | Desktop {memory :: Int, processor :: Int}

==>

setWeight v x = x{weight=v}
setMemory v x = x{memory=v}
setProcessor v x = x{process=v}

-}

makeSet = Derivation set' "Set"

set' dat = map f fields
    where
        ctors = dataCtors dat
        fields = nub $ concatMap ctorFields ctors
        
        f field = funN ("set" ++ toUpper (head field) : tail field) $
                       [sclause [vr "v",vr "x"] $ RecUpdE (vr "x") [(mkName field,vr "v")]]
