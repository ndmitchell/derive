{-# OPTIONS_GHC -fth -cpp #-}

-- | Derivation for 'Enum', as defined by the Haskell 98 Language
-- report, except that we support arbitrary types.  toEnum (and
-- derived functions, notably succ and pred) use undefined to fill all
-- fields of nonzero arity constructors.
module Data.Derive.Enum(makeEnum) where

import Language.Haskell.TH.All


#ifdef GUESS

import Data.DeriveGuess

example = (,) "Enum" [d|

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

    |]

#endif

makeEnum :: Derivation
makeEnum = Derivation enum' "Enum"
enum' dat = [instance_context [] "Enum" dat [FunD (mkName "toEnum") ((map (\(
    ctorInd,ctor) -> (Clause [(LitP (IntegerL ctorInd))] (NormalB ((flip
    RecConE []) (mkName (ctorName ctor)))) [])) (id (zip [0..] (dataCtors dat))
    ))++[(Clause [(VarP (mkName "n"))] (NormalB (applyWith (VarE (mkName "$"))
    [(VarE (mkName "error")),(applyWith (VarE (mkName "++")) [(LitE (StringL
    "toEnum ")),(applyWith (VarE (mkName "++")) [(AppE (VarE (mkName "show")) (
    VarE (mkName "n"))),(applyWith (VarE (mkName "++")) [(LitE (StringL
    ", not defined for ")),(LitE (StringL (dataName dat)))])])])])) [])]++[]),
    FunD (mkName "fromEnum") ((map (\(ctorInd,ctor) -> (Clause [((flip RecP [])
    (mkName (ctorName ctor)))] (NormalB (LitE (IntegerL ctorInd))) [])) (id (
    zip [0..] (dataCtors dat))))++[])]]
