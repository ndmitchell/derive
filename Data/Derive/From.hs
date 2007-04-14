{-# OPTIONS_GHC -fth -cpp #-}

module Data.Derive.From(makeFrom) where

import Language.Haskell.TH.All


#ifdef GUESS

import Data.DeriveGuess

example = (,) "From" [d|

    fromCtorZero (CtorZero) = ()
    fromCtorOne  (CtorOne x1) = tup1 x1
    fromCtorTwo  (CtorTwo x1 x2) = (x1,x2)
    fromCtorTwo' (CtorTwo' x1 x2) = (x1,x2)

    |]

#endif


makeFrom = Derivation from' "From"
from' dat = ((map (\(ctorInd,ctor) -> (FunD (mkName ("from" ++ ctorName ctor))
    [(Clause [(ConP (mkName ("" ++ ctorName ctor)) ((map (\field -> (VarP (
    mkName ("x" ++ show field)))) (id [1..ctorArity ctor]))++[]))] (NormalB (
    TupE ((map (\field -> (VarE (mkName ("x" ++ show field)))) (id [1..
    ctorArity ctor]))++[]))) [])])) (id (zip [0..] (dataCtors dat))))++[])
