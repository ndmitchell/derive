{-# OPTIONS_GHC -fth -fno-warn-missing-methods -cpp #-}

-- | Derivation for 'Test.QuickCheck.Arbitrary'.
--
-- Things to note:
--
-- * The resulting instances do not implement @coarbitrary@, only
-- @arbitrary@.
--
-- * The resulting instances of @arbitrary@ generate each constructor
-- of the data type with equal probability.
--
-- * No form of size control is used.
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

        coarbitrary CtorZero = variant 0
        coarbitrary (CtorOne x1) = variant 1 . coarbitrary x1
        coarbitrary (CtorTwo x1 x2) = variant 2 . coarbitrary x1 . coarbitrary x2
        coarbitrary (CtorTwo' x1 x2) = variant 3 . coarbitrary x1 . coarbitrary x2
    |]

#endif

makeArbitrary :: Derivation
makeArbitrary = derivation arbitrary' "Arbitrary"
arbitrary' dat = [InstanceD (concat ([(map (\tdat -> (AppT (ConT (mkName
    "Arbitrary")) tdat)) (dataVars dat))])) (head [(AppT (ConT (mkName
    "Arbitrary")) (lK (dataName dat) (dataVars dat)))])[(ValD (VarP (mkName
    "arbitrary")) (NormalB (DoE [(BindS (VarP (mkName "x")) (AppE (VarE (mkName
    "choose")) (TupE [(LitE (IntegerL 0)),(LitE (IntegerL (toInteger (length (
    dataCtors dat) - 1))))]))),(NoBindS (CaseE (VarE (mkName "x")) ((map (\(
    ctorInd,ctor) -> (Match (LitP (IntegerL ctorInd)) (NormalB (DoE ((map (
    \field -> (BindS (VarP (mkName ("x" ++ show field))) (VarE (mkName
    "arbitrary")))) (id [1..ctorArity ctor]))++[(NoBindS (AppE (VarE (mkName
    "return")) (applyWith (ConE (mkName ("" ++ ctorName ctor))) ((map (\field
    -> (VarE (mkName ("x" ++ show field)))) (id [1..ctorArity ctor]))++[]))))]
    ++[]))) [])) (id (zip [0..] (dataCtors dat))))++[])))])) []),(FunD (mkName
    "coarbitrary") ((map (\(ctorInd,ctor) -> (Clause [(ConP (mkName ("" ++
    ctorName ctor)) ((map (\field -> (VarP (mkName ("x" ++ show field)))) (id [
    1..ctorArity ctor]))++[]))] (NormalB (foldl1With (VarE (mkName ".")) ([(
    AppE (VarE (mkName "variant")) (LitE (IntegerL ctorInd)))]++(map (\field ->
    (AppE (VarE (mkName "coarbitrary")) (VarE (mkName ("x" ++ show field))))) (
    id [1..ctorArity ctor]))++[]))) [])) (id (zip [0..] (dataCtors dat))))++[])
    )]]
