{-# OPTIONS_GHC -fth #-}

module Data.Derive.Eq(makeEq) where

import Data.DeriveGuess
import Language.Haskell.TH.All

makeEq = Derivation eq' "Eq"
eq' dat = simple_instance "Eq" dat [funN "==" body]
    where
        body = map rule (dataCtors dat) ++ [defclause 2 false]

rule ctor = sclause [ctp ctor 'a', ctp ctor 'b']
                    (and' (zipWith (==:) (ctv ctor 'a') (ctv ctor 'b')))


example = [d|

    instance (Eq a, Eq b) => Eq (Data2 a b) where
        Ctor0 == Ctor0 = True
        (Ctor1 x) == (Ctor1 y) = x == y
        (Ctor2 x1 x2) == (Ctor2 y1 y2) = x1 == y1 && x2 == y2
        _ == _ = False

    |]

