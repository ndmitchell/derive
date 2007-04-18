{-# OPTIONS_GHC -fth -cpp #-}

module Data.Derive.Bounded(makeBounded) where

import Language.Haskell.TH.All


#ifdef GUESS

import Data.DeriveGuess

example = (,) "Bounded" [d|

    instance Bounded a => Bounded (DataName a) where
        minBound = head [CtorZero, CtorOne minBound, CtorTwo minBound minBound, CtorTwo' minBound minBound]
        maxBound = head [CtorTwo' maxBound maxBound, CtorTwo maxBound maxBound, CtorOne maxBound, CtorZero]

    |]

#endif


makeBounded = Derivation bounded' "Bounded"
bounded' dat = [instance_context ["Bounded"] "Bounded" dat [(ValD (VarP (mkName
    "minBound")) (NormalB (AppE (VarE (mkName "head")) (ListE ((map (\(ctorInd,
    ctor) -> (applyWith (ConE (mkName ("" ++ ctorName ctor))) (replicate (
    ctorArity ctor) (VarE (mkName "minBound"))))) (id (zip [0..] (dataCtors dat
    ))))++[])))) []),(ValD (VarP (mkName "maxBound")) (NormalB (AppE (VarE (
    mkName "head")) (ListE ((map (\(ctorInd,ctor) -> (applyWith (ConE (mkName (
    "" ++ ctorName ctor))) (replicate (ctorArity ctor) (VarE (mkName "maxBound"
    ))))) (reverse (zip [0..] (dataCtors dat))))++[])))) [])]]
