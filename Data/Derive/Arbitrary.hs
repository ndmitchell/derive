{-# OPTIONS_GHC -fth -fno-warn-missing-methods -cpp #-}

module Data.Derive.Arbitrary(makeArbitrary) where

import Language.Haskell.TH.All


#ifdef GUESS

import Test.QuickCheck
import Data.DeriveGuess

example = (,) "Arbitrary" [d|


    instance Arbitrary a => Arbitrary (DataName a) where
        arbitrary = do
            x <- choose (0,3)
            case x of
                0 -> do return CtorZero
                1 -> do x1 <- arbitrary
                        return (CtorOne x1)
                2 -> do x1 <- arbitrary
                        x2 <- arbitrary
                        return (CtorTwo x1 x2)
                3 -> do x1 <- arbitrary
                        x2 <- arbitrary
                        return (CtorTwo' x1 x2)

        coarbitrary = error "coarbitrary not yet supported"

    |]

#endif


makeArbitrary = Derivation arbitrary' "Arbitrary"
arbitrary' dat = [instance_context ["Arbitrary"] "Arbitrary" dat [(ValD (VarP (
    mkName "arbitrary")) (NormalB (DoE [(BindS (VarP (mkName "x")) (AppE (VarE
    (mkName "choose")) (TupE [(LitE (IntegerL 0)),(LitE (IntegerL (toInteger (
    length (dataCtors dat) - 1))))]))),(NoBindS (CaseE (VarE (mkName "x")) ((
    map (\(ctorInd,ctor) -> (Match (LitP (IntegerL ctorInd)) (NormalB (DoE ((
    map (\field -> (BindS (VarP (mkName ("x" ++ show field))) (VarE (mkName
    "arbitrary")))) (id [1..ctorArity ctor]))++[(NoBindS (AppE (VarE (mkName
    "return")) (applyWith (ConE (mkName ("" ++ ctorName ctor))) ((map (\field
    -> (VarE (mkName ("x" ++ show field)))) (id [1..ctorArity ctor]))++[]))))]
    ++[]))) [])) (id (zip [0..] (dataCtors dat))))++[])))])) []),(ValD (VarP (
    mkName "coarbitrary")) (NormalB (AppE (VarE (mkName "error")) (LitE (
    StringL "coarbitrary not yet supported")))) [])]]
