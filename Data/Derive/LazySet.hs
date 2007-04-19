
module Data.Derive.LazySet(makeLazySet) where

import Language.Haskell.TH.All
import Data.Char
import Data.List

{-
data State = State {x :: Int, y :: Int}

==>

setX a0 b0 = State a0 (y b0)
setY a0 b0 = State (x b0) a0

-}

makeLazySet = Derivation lazyset' "LazySet"

lazyset' dat = map f fields
    where
        fields = nub $ concatMap (\f -> map ((,) f) (ctorFields f)) (dataCtors dat)

        f (ctor,field) = funN name [sclause [vr "a0", vr "b0"] (lK (ctorName ctor) body)]
            where
                name = "set" ++ toUpper (head field) : tail field
                body = [ if fld == field then vr "a0" else l1 fld (vr "b0")
                             | fld <- ctorFields ctor ]
