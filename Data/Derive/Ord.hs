{-# OPTIONS_GHC -fth -cpp #-}

module Data.Derive.Ord(makeOrd) where

import Language.Haskell.TH.All

#ifdef GUESS

import Data.DeriveGuess

instance Eq (DataName a) where

example = [d|

    instance Ord a => Ord (DataName a) where
        compare a b = check a b
            where
                check CtorZero CtorZero = EQ
                check (CtorOne x1) (CtorOne y1) = compare x1 y1 `thenCmp` EQ
                check (CtorTwo x1 x2) (CtorTwo y1 y2) = compare x1 y1 `thenCmp` compare x2 y2 `thenCmp` EQ
                check (CtorTwo' x1 x2) (CtorTwo' y1 y2) = compare x1 y1 `thenCmp` compare x2 y2 `thenCmp` EQ
                check a b = compare (tag a) (tag b)
                
                tag (CtorZero{}) = 0
                tag (CtorOne{}) = 1
                tag (CtorTwo{}) = 2
                tag (CtorTwo'{}) = 3
                
                EQ `thenCmp` o2 = o2
                o1 `thenCmp` _  = o1

    |]

#endif


makeOrd = Derivation ord' "Ord"
ord' dat = instance_context ["Ord"] "Ord" dat [FunD (mkName "compare") [(Clause
    [(VarP (mkName "a")),(VarP (mkName "b"))] (NormalB (AppE (AppE (VarE (
    mkName "check")) (VarE (mkName "a"))) (VarE (mkName "b")))) [FunD (mkName
    "check") ((map (\(ctorInd,ctor) -> (Clause [(ConP (mkName (ctorName ctor))
    ((map (\field -> (VarP (mkName ("x" ++ show field)))) (id [1..ctorArity
    ctor]))++[])),(ConP (mkName (ctorName ctor)) ((map (\field -> (VarP (mkName
    ("y" ++ show field)))) (id [1..ctorArity ctor]))++[]))] (NormalB (
    foldr1With (VarE (mkName "thenCmp")) ([(ConE (mkName "EQ"))]++(map (\field
    -> (AppE (AppE (VarE (mkName "compare")) (VarE (mkName ("x" ++ show field))
    )) (VarE (mkName ("y" ++ show field))))) (reverse [1..ctorArity ctor]))++[]
    ))) [])) (id (zip [0..] (dataCtors dat))))++[(Clause [(VarP (mkName "a")),(
    VarP (mkName "b"))] (NormalB (AppE (AppE (VarE (mkName "compare")) (AppE (
    VarE (mkName "tag")) (VarE (mkName "a")))) (AppE (VarE (mkName "tag")) (
    VarE (mkName "b"))))) [])]++[]),FunD (mkName "tag") ((map (\(ctorInd,ctor)
    -> (Clause [((flip RecP []) (mkName (ctorName ctor)))] (NormalB (LitE (
    IntegerL ctorInd))) [])) (id (zip [0..] (dataCtors dat))))++[]),FunD (
    mkName "thenCmp") [(Clause [(ConP (mkName "EQ") []),(VarP (mkName "o2"))] (
    NormalB (VarE (mkName "o2"))) []),(Clause [(VarP (mkName "o1")),WildP] (
    NormalB (VarE (mkName "o1"))) [])]])]]

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
