{-# OPTIONS_GHC -fth -cpp #-}

-- | A pseudo derivation.  For each constructor of the data type, @Is@
-- generates @is@/CtorName/ which returns 'True' if given an object
-- build using the appropriate constructor, and 'False' otherwise.
module Data.Derive.Is(makeIs) where

import Language.Haskell.TH.All


#ifdef GUESS

import Data.DeriveGuess

example = (,) "Is" [d|

    isCtorZero (CtorZero{}) = True; isCtorZero _ = False
    isCtorOne  (CtorOne {}) = True; isCtorOne  _ = False
    isCtorTwo  (CtorTwo {}) = True; isCtorTwo  _ = False
    isCtorTwo' (CtorTwo'{}) = True; isCtorTwo' _ = False

    |]

#endif

makeIs :: Derivation
makeIs = derivation is' "Is"
is' dat = ((map (\(ctorInd,ctor) -> (FunD (mkName ("is" ++ ctorName ctor)) [(
    Clause [((flip RecP []) (mkName ("" ++ ctorName ctor)))] (NormalB (ConE (
    mkName "True"))) []),(Clause [WildP] (NormalB (ConE (mkName "False"))) [])]
    )) (id (zip [0..] (dataCtors dat))))++[])
