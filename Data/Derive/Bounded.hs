{-# OPTIONS_GHC -fth -cpp #-}

module Data.Derive.Bounded(makeBounded) where

import Language.Haskell.TH.All


#ifdef GUESS

import Data.DeriveGuess

example = [d|

    instance Bounded (DataName a) where
        minBound = head [CtorZero{}, CtorOne{}, CtorTwo{}, CtorTwo'{}]
        maxBound = head $ reverse [CtorZero{}, CtorOne{}, CtorTwo{}, CtorTwo'{}]

    |]

#endif


makeBounded = Derivation bounded' "Bounded"
bounded' dat = [instance_context [] "Bounded" dat [ValD (VarP (mkName "minBound"
    )) (NormalB (AppE (VarE (mkName "head")) (ListE ((map (\(ctorInd,ctor) -> (
    (flip RecConE []) (mkName (ctorName ctor)))) (id (zip [0..] (dataCtors dat)
    )))++[])))) [],ValD (VarP (mkName "maxBound")) (NormalB (applyWith (VarE (
    mkName "$")) [(VarE (mkName "head")),(AppE (VarE (mkName "reverse")) (ListE
    ((map (\(ctorInd,ctor) -> ((flip RecConE []) (mkName (ctorName ctor)))) (id
    (zip [0..] (dataCtors dat))))++[])))])) []]]
