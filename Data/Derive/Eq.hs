{-# OPTIONS_GHC -fth #-}
module Data.Derive.Eq(eq) where

import Data.Derive
import Data.List
import Data.Char
import Language.Haskell.TH

eq = Derivation eq' "Eq"
eq' dat = simple_instance ''Eq dat [FunD '(==) body]
    where
        body = map rule (dataCtors dat) ++ [defclause 2 false]

rule ctor = sclause (map (lK (ctorName ctor) . na) "ab")
                    (and' (zipWith (==:) (na 'a') (na 'b')))
    where
        na c = map (vr . (c:) . show) [1 .. ctorArity ctor]
