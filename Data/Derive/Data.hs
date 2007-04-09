{-# OPTIONS_GHC -fth -fno-warn-missing-methods -cpp #-}

module Data.Derive.Data(makeData) where

import Language.Haskell.TH.All


#ifdef GUESS

import Data.Generics
import Data.DeriveGuess

instance Typeable (Data2 a b) where

example = [d|

    instance (Data a, Typeable a, Data b, Typeable b) => Data (Data2 a b) where
        gfoldl k r Ctor0 = r Ctor0
        gfoldl k r (Ctor1 x1) = r Ctor1 `k` x1
        gfoldl k r (Ctor2 x1 x2) = r Ctor2 `k` x1 `k` x2

    |]

#endif


makeData = Derivation data' "Data"
data' dat = instance_context ["Data","Typeable"] "Data" dat [funN "gfoldl" (map
    (\c -> Clause [VarP (mkName "k"),VarP (mkName "r"),ctp c 'x'] (NormalB (
    foldl (\a b -> AppE (AppE ((VarE (mkName "k"))) a) b) (AppE ((VarE (mkName
    "r"))) ((ConE (mkName (ctorName c))))) (map (\(VarE q) -> VarE q) (ctv c
    'x')))) []) (dataCtors dat))]
