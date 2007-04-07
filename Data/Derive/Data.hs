{-# OPTIONS_GHC -fth -fno-warn-missing-methods #-}

module Data.Derive.Data(makeData) where

import Data.Derive
import Data.Generics
import Data.Derive.Example
import Language.Haskell.TH

makeData = Derivation data' "Data"
data' dat = simple_instance "Data" dat [funN "gfoldl" body]
    where
        body = map f $ dataCtors dat
        
        f ctor = sclause [vr "k",vr "z",ctp ctor 'x']
                         (foldl f (AppE (vr "z") (l0 (ctorName ctor))) (ctv ctor 'x'))
            where
                f x y = AppE (AppE (vr "k") x) y



instance Typeable (Data2 a b) where


example = [d|

    instance (Data a, Typeable a, Data b, Typeable b) => Data (Data2 a b) where
        gfoldl k r Ctor0 = r Ctor0
        gfoldl k r (Ctor1 x1) = r Ctor1 `k` x1
        gfoldl k r (Ctor2 x1 x2) = r Ctor2 `k` x1 `k` x2

    |]

