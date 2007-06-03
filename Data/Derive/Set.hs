
-- | A pseudo derivation.  For each field in the data type, deriving
-- @Set@ generates @set@/FieldName/@ v x = x{@/fieldName/@ = v}@.
-- This derivation is intended to work around the fact that in Haskell
-- assigning to a field is not a first class object (although
-- extracting from a field is).
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

makeSet :: Derivation
makeSet = derivation set' "Set"

set' dat = map f fields
    where
        ctors = dataCtors dat
        fields = nub $ concatMap ctorFields ctors
        
        f field = funN ("set" ++ toUpper (head field) : tail field) $
                       [sclause [vr "v",vr "x"] $ RecUpdE (vr "x") [(mkName field,vr "v")]]
