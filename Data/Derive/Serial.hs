{-# OPTIONS_GHC -fth -fno-warn-missing-methods -cpp #-}

-- | Derivation for the @Serial@ class used by SmallCheck.  Following
-- the guidelines in the SmallCheck README
-- <http://www.cs.york.ac.uk/fp/darcs/smallcheck/README>, we implement
-- both @series@ and @coseries@.  The generated instances use the
-- SmallCheck instance combinators in the proscribed way.

module Data.Derive.Serial(makeSerial) where

import Language.Haskell.TH.All


#ifdef GUESS

import SmallCheck
import Data.DeriveGuess

example = (,) "Serial" [d|

    instance Serial a => Serial (DataName a) where
        series = cons0 CtorZero \/
                 cons1 CtorOne  \/
                 cons2 CtorTwo  \/
                 cons2 CtorTwo'

        coseries d = [ \t -> case t of
                                CtorZero -> t0
                                CtorOne x1 -> t1 x1
                                CtorTwo x1 x2 -> t2 x1 x2
                                CtorTwo' x1 x2 -> t3 x1 x2
                     | t0 <- alts0 d
                     , t1 <- alts1 d
                     , t2 <- alts2 d
                     , t3 <- alts2 d
                     ]

    |]

#endif

makeSerial :: Derivation
makeSerial = Derivation serial' "Serial"
serial' dat = [instance_context ["Serial"] "Serial" dat [ValD (VarP (mkName
    "series")) (NormalB (foldl1With (VarE (mkName "\\/")) ((map (\(ctorInd,ctor
    ) -> (AppE (VarE (mkName ("cons" ++ show (ctorArity ctor)))) (ConE (mkName
    (ctorName ctor))))) (id (zip [0..] (dataCtors dat))))++[]))) [],FunD (
    mkName "coseries") [(Clause [(VarP (mkName "d"))] (NormalB (CompE ((map (\(
    ctorInd,ctor) -> (BindS (VarP (mkName ("t" ++ show ctorInd))) (AppE (VarE (
    mkName ("alts" ++ show (ctorArity ctor)))) (VarE (mkName "d"))))) (id (zip
    [0..] (dataCtors dat))))++[(NoBindS (LamE [(VarP (mkName "t"))] (CaseE (
    VarE (mkName "t")) ((map (\(ctorInd,ctor) -> (Match (ConP (mkName (ctorName
    ctor)) ((map (\field -> (VarP (mkName ("x" ++ show field)))) (id [1..
    ctorArity ctor]))++[])) (NormalB (applyWith (VarE (mkName ("t" ++ show
    ctorInd))) ((map (\field -> (VarE (mkName ("x" ++ show field)))) (id [1..
    ctorArity ctor]))++[]))) [])) (id (zip [0..] (dataCtors dat))))++[]))))]++[
    ]))) [])]]]
