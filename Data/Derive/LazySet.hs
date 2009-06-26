-- | A pseudo derivation.  For each field in the data type, deriving
-- @LazySet@ generates a function like a record updator, but lazy.
-- This is very useful in certain situations to improve laziness
-- properties.  Example:
--
-- > data Foo = Foo { x :: Int, y :: Int, z :: Int }
--
-- becomes:
--
-- > setX v f = Foo v (y f) (z f)
-- > setY v f = Foo (x f) v (z f)
-- > setZ v f = Foo (x f) (y f) v
module Data.Derive.LazySet(makeLazySet) where

{-
-}

import Language.Haskell.TH.All
import Data.Char
import Data.List

{-
data State = State {x :: Int, y :: Int}

==>

setX a0 b0 = State a0 (y b0)
setY a0 b0 = State (x b0) a0

-}

makeLazySet :: Derivation
makeLazySet = derivation lazyset' "LazySet"

lazyset' dat = map f fields
    where
        fields = nub $ concatMap (\f -> map ((,) f) (ctorFields f)) (dataCtors dat)

        f (ctor,field) = funN name [sclause [vr "a0", vr "b0"] (lK (ctorName ctor) body)]
            where
                name = "set" ++ toUpper (head field) : tail field
                body = [ if fld == field then vr "a0" else l1 fld (vr "b0")
                             | fld <- ctorFields ctor ]
