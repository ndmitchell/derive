{-# OPTIONS_GHC -fth -fno-warn-missing-methods -cpp #-}

module Data.Derive.Data(makeData) where

import Language.Haskell.TH.All


#ifdef GUESS

import Data.Generics
import Data.DeriveGuess

instance Typeable (DataName a) where

example = [d|

    instance (Data a, Typeable a) => Data (DataName a) where
        gfoldl k r CtorZero = r CtorZero
        gfoldl k r (CtorOne x1) = r CtorOne `k` x1
        gfoldl k r (CtorTwo x1 x2) = r CtorTwo `k` x1 `k` x2
        gfoldl k r (CtorTwo' x1 x2) = r CtorTwo' `k` x1 `k` x2

    |]

#endif


makeData = Derivation data' "Data"

data' dat = [instance_context ["Data","Typeable"] "Data" dat [FunD (mkName
    "gfoldl") ((map (\(ctorInd,ctor) -> (Clause [(VarP (mkName "k")),(VarP (
    mkName "r")),(ConP (mkName (ctorName ctor)) ((map (\field -> (VarP (mkName
    ("x" ++ show field)))) (id [1..ctorArity ctor]))++[]))] (NormalB (
    foldr1With (VarE (mkName "k")) ((map (\field -> (VarE (mkName ("x" ++ show
    field)))) (reverse [1..ctorArity ctor]))++[(AppE (VarE (mkName "r")) (ConE
    (mkName (ctorName ctor))))]++[]))) [])) (id (zip [0..] (dataCtors dat))))++
    [])]]
