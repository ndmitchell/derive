{-# OPTIONS_GHC -fth -cpp #-}

-- | Derive @Bounded@, as specified in the Haskell 98 Language Report.
-- As an extension, we support deriving @Bounded@ for all data types.
-- If the first or last constructor has non-zero arity, we call
-- minBound (respectively, maxBound) recursively to fill in the
-- fields.
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

makeBounded :: Derivation
makeBounded = derivation bounded' "Bounded"
bounded' dat = [instance_context ["Bounded"] "Bounded" dat [(ValD (VarP (mkName
    "minBound")) (NormalB (AppE (VarE (mkName "head")) (ListE ((map (\(ctorInd,
    ctor) -> (applyWith (ConE (mkName ("" ++ ctorName ctor))) (replicate (
    ctorArity ctor) (VarE (mkName "minBound"))))) (id (zip [0..] (dataCtors dat
    ))))++[])))) []),(ValD (VarP (mkName "maxBound")) (NormalB (AppE (VarE (
    mkName "head")) (ListE ((map (\(ctorInd,ctor) -> (applyWith (ConE (mkName (
    "" ++ ctorName ctor))) (replicate (ctorArity ctor) (VarE (mkName "maxBound"
    ))))) (reverse (zip [0..] (dataCtors dat))))++[])))) [])]]
