{-# OPTIONS_GHC -fth -fno-warn-missing-methods -cpp #-}

-- | Derive NFData, from Control.Parallel.Strategies.
module Data.Derive.NFData(makeNFData) where

import Language.Haskell.TH.All


#ifdef GUESS

import Control.Parallel.Strategies
import Data.DeriveGuess

example = (,) "NFData" [d|

    instance (NFData a) => NFData (DataName a) where
        rnf CtorZero = ()
        rnf (CtorOne x1) = rnf x1 `seq` ()
        rnf (CtorTwo x1 x2) = rnf x1 `seq` rnf x2 `seq` ()
        rnf (CtorTwo' x1 x2) = rnf x1 `seq` rnf x2 `seq` ()
    |]

#endif

makeNFData :: Derivation
makeNFData = Derivation nFData' "NFData"
nFData' dat = [instance_context ["NFData"] "NFData" dat [(FunD (mkName "rnf") (
    (map (\(ctorInd,ctor) -> (Clause [(ConP (mkName ("" ++ ctorName ctor)) ((
    map (\field -> (VarP (mkName ("x" ++ show field)))) (id [1..ctorArity ctor]
    ))++[]))] (NormalB (foldl1With (VarE (mkName "seq")) ((map (\field -> (AppE
    (VarE (mkName "rnf")) (VarE (mkName ("x" ++ show field))))) (id [1..
    ctorArity ctor]))++[(TupE [])]++[]))) [])) (id (zip [0..] (dataCtors dat)))
    )++[]))]]
