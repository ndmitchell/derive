{-# OPTIONS_GHC -fth -cpp #-}

-- | Derive 'Ord', as specified in the Haskell 98 Language Report.
module Data.Derive.Ord(makeOrd) where

import Language.Haskell.TH.All

#ifdef GUESS

import Data.DeriveGuess

instance Eq (DataName a) where

example = (,) "Ord" [d|

    instance Ord a => Ord (DataName a) where
        compare a b = check a b
            where
                check CtorZero CtorZero = compare () ()
                check (CtorOne x1) (CtorOne y1) = compare (tup1 x1) (tup1 y1)
                check (CtorTwo x1 x2) (CtorTwo y1 y2) = compare (x1,x2) (y1,y2)
                check (CtorTwo' x1 x2) (CtorTwo' y1 y2) = compare (x1,x2) (y1,y2)
                check a b = compare (tag a) (tag b)
                
                tag (CtorZero{}) = 0
                tag (CtorOne{}) = 1
                tag (CtorTwo{}) = 2
                tag (CtorTwo'{}) = 3

    |]

#endif

makeOrd :: Derivation
makeOrd = Derivation ord' "Ord"
ord' dat = [instance_context ["Ord"] "Ord" dat [FunD (mkName "compare") [(Clause
    [(VarP (mkName "a")),(VarP (mkName "b"))] (NormalB (applyWith (VarE (mkName
    "check")) [(VarE (mkName "a")),(VarE (mkName "b"))])) [FunD (mkName "check"
    ) ((map (\(ctorInd,ctor) -> (Clause [(ConP (mkName (ctorName ctor)) ((map (
    \field -> (VarP (mkName ("x" ++ show field)))) (id [1..ctorArity ctor]))++[
    ])),(ConP (mkName (ctorName ctor)) ((map (\field -> (VarP (mkName ("y" ++
    show field)))) (id [1..ctorArity ctor]))++[]))] (NormalB (applyWith (VarE (
    mkName "compare")) [(TupE ((map (\field -> (VarE (mkName ("x" ++ show field
    )))) (id [1..ctorArity ctor]))++[])),(TupE ((map (\field -> (VarE (mkName (
    "y" ++ show field)))) (id [1..ctorArity ctor]))++[]))])) [])) (id (zip [0..
    ] (dataCtors dat))))++[(Clause [(VarP (mkName "a")),(VarP (mkName "b"))] (
    NormalB (applyWith (VarE (mkName "compare")) [(AppE (VarE (mkName "tag")) (
    VarE (mkName "a"))),(AppE (VarE (mkName "tag")) (VarE (mkName "b")))])) [])
    ]++[]),FunD (mkName "tag") ((map (\(ctorInd,ctor) -> (Clause [((flip RecP [
    ]) (mkName (ctorName ctor)))] (NormalB (LitE (IntegerL ctorInd))) [])) (id
    (zip [0..] (dataCtors dat))))++[])])]]]

{-
-- HAND WRITTEN VERSION
-- requires O(n^2) page space (the automatic one is O(n))

ord' dat = simple_instance "Ord" dat [funN "compare" body]
    where
        obs  = zip [0..] (dataCtors dat)
        body = [ sclause [ctp (snd x) 'a', ctp (snd y) 'b'] (rule x y)
                     | x <- obs , y <- obs ]

rule (i1,c1) (i2,c2) | i1 < i2   = l0 "LT"
                     | i1 > i2   = l0 "GT"
                     | otherwise = l2 "compare" (tup2 c1 'a') (tup2 c2 'b')

tup2 c ch = foldr (l2 "(,)") (lit ()) (ctv c ch)
-}
