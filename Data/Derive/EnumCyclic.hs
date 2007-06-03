{-# OPTIONS_GHC -fth -cpp #-}

-- | @EnumCyclic@ defines the @Enum@ class, using the same
-- modifications as our @Enum@ derivation, but additionally @succ@
-- and @pred@ treat the data type as cyclic, wrapping between the
-- first and last constructors.

module Data.Derive.EnumCyclic(makeEnumCyclic) where

import Language.Haskell.TH.All


#ifdef GUESS

import Data.DeriveGuess

example = (,) "EnumCyclic" [d|

    instance Enum (DataName a) where
        toEnum 0 = CtorZero{}
        toEnum 1 = CtorOne {}
        toEnum 2 = CtorTwo {}
        toEnum 3 = CtorTwo'{}
        toEnum n = error $ "toEnum " ++ show n ++ ", not defined for " ++ "DataName"
        
        fromEnum (CtorZero{}) = 0
        fromEnum (CtorOne {}) = 1
        fromEnum (CtorTwo {}) = 2
        fromEnum (CtorTwo'{}) = 3
        
        
        succ a = if b == 3 then toEnum 0 else toEnum (b+1)
            where b = fromEnum a

        pred a = if b == 0 then toEnum 3 else toEnum (b-1)
            where b = fromEnum a

    |]

#endif

makeEnumCyclic :: Derivation
makeEnumCyclic = derivation enumCyclic' "EnumCyclic"
enumCyclic' dat = [instance_context [] "Enum" dat [(FunD (mkName "toEnum") ((
    map (\(ctorInd,ctor) -> (Clause [(LitP (IntegerL ctorInd))] (NormalB ((flip
    RecConE []) (mkName ("" ++ ctorName ctor)))) [])) (id (zip [0..] (dataCtors
    dat))))++[(Clause [(VarP (mkName "n"))] (NormalB (applyWith (VarE (mkName
    "$")) [(VarE (mkName "error")),(applyWith (VarE (mkName "++")) [(LitE (
    StringL "toEnum ")),(applyWith (VarE (mkName "++")) [(AppE (VarE (mkName
    "show")) (VarE (mkName "n"))),(applyWith (VarE (mkName "++")) [(LitE (
    StringL ", not defined for ")),(LitE (StringL (dataName dat)))])])])])) [])
    ]++[])),(FunD (mkName "fromEnum") ((map (\(ctorInd,ctor) -> (Clause [((flip
    RecP []) (mkName ("" ++ ctorName ctor)))] (NormalB (LitE (IntegerL ctorInd)
    )) [])) (id (zip [0..] (dataCtors dat))))++[])),(FunD (mkName "succ") [(
    Clause [(VarP (mkName "a"))] (NormalB (CondE (applyWith (VarE (mkName "==")
    ) [(VarE (mkName "b")),(LitE (IntegerL (toInteger (length (dataCtors dat)))
    ))]) (AppE (VarE (mkName "toEnum")) (LitE (IntegerL 0))) (AppE (VarE (
    mkName "toEnum")) (applyWith (VarE (mkName "+")) [(VarE (mkName "b")),(LitE
    (IntegerL 1))])))) [(ValD (VarP (mkName "b")) (NormalB (AppE (VarE (mkName
    "fromEnum")) (VarE (mkName "a")))) [])])]),(FunD (mkName "pred") [(Clause [
    (VarP (mkName "a"))] (NormalB (CondE (applyWith (VarE (mkName "==")) [(VarE
    (mkName "b")),(LitE (IntegerL 0))]) (AppE (VarE (mkName "toEnum")) (LitE (
    IntegerL (toInteger (length (dataCtors dat)))))) (AppE (VarE (mkName
    "toEnum")) (applyWith (VarE (mkName "-")) [(VarE (mkName "b")),(LitE (
    IntegerL 1))])))) [(ValD (VarP (mkName "b")) (NormalB (AppE (VarE (mkName
    "fromEnum")) (VarE (mkName "a")))) [])])])]]
