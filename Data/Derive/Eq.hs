{-# OPTIONS_GHC -fth -cpp #-}

-- | Derive 'Eq', as specified in the Haskell 98 Language Report.
module Data.Derive.Eq(makeEq) where

import Language.Haskell.TH.All


#ifdef GUESS

import Data.DeriveGuess

example = (,) "Eq" [d|

    instance Eq a => Eq (DataName a) where
        CtorZero == CtorZero = True
        (CtorOne x1) == (CtorOne y1) = x1 == y1 && True
        (CtorTwo x1 x2) == (CtorTwo y1 y2) = x1 == y1 && x2 == y2 && True
        (CtorTwo' x1 x2) == (CtorTwo' y1 y2) = x1 == y1 && x2 == y2 && True
        _ == _ = False

    |]

#endif

makeEq :: Derivation
makeEq = derivation eq' "Eq"
eq' dat = [instance_context ["Eq"] "Eq" dat [FunD (mkName "==") ((map (\(ctorInd
    ,ctor) -> (Clause [(ConP (mkName (ctorName ctor)) ((map (\field -> (VarP (
    mkName ("x" ++ show field)))) (id [1..ctorArity ctor]))++[])),(ConP (mkName
    (ctorName ctor)) ((map (\field -> (VarP (mkName ("y" ++ show field)))) (id
    [1..ctorArity ctor]))++[]))] (NormalB (foldl1With (VarE (mkName "&&")) ((
    map (\field -> (AppE (AppE (VarE (mkName "==")) (VarE (mkName ("x" ++ show
    field)))) (VarE (mkName ("y" ++ show field))))) (id [1..ctorArity ctor]))++
    [(ConE (mkName "True"))]++[]))) [])) (id (zip [0..] (dataCtors dat))))++[(
    Clause [WildP,WildP] (NormalB (ConE (mkName "False"))) [])]++[])]]


{-
-- HAND WRITTEN VERSION

eq' dat = simple_instance "Eq" dat [funN "==" body]
    where
        body = map rule (dataCtors dat) ++ [defclause 2 false]

rule ctor = sclause [ctp ctor 'a', ctp ctor 'b']
                    (and_ (zipWith (==:) (ctv ctor 'a') (ctv ctor 'b')))
-}
