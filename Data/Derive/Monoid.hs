
module Data.Derive.Monoid(makeMonoid) where

import Language.Haskell.TH.All

{-
data Foo = Foo a String

==>

instance Monoid a => Monoid (Foo a) where
    mempty = Foo mempty mempty
    mappend (Foo x1 x2) (Foo y1 y2) = Foo (mappend x1 y1) (mappend x2 y2)

-}

makeMonoid = Derivation monoid' "Monoid"

monoid' dat | length (dataCtors dat) == 1
         = [instance_default "Monoid" dat [funN "mempty" [empty],funN "mappend" [append]]]
    where
        ctor = head $ dataCtors dat

        empty  = sclause [] $ lK (ctorName ctor) (replicate (ctorArity ctor) (l0 "mempty"))
        append = sclause [ctp ctor 'x',ctp ctor 'y'] $
                         lK (ctorName ctor) (zipWith (l2 "mappend") (ctv ctor 'x') (ctv ctor 'y'))

monoid' dat = []
