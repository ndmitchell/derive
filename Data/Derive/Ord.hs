{-# OPTIONS_GHC -fth -cpp #-}

-- | Derive 'Ord', as specified in the Haskell 98 Language Report.
module Data.Derive.Ord(makeOrd) where

import Control.Monad(guard)
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
                check x y = compare (tag x) (tag y)
                
                tag (CtorZero{}) = 0
                tag (CtorOne{}) = 1
                tag (CtorTwo{}) = 2
                tag (CtorTwo'{}) = 3

    |]

#endif

makeOrd :: Derivation
makeOrd = derivation ord' "Ord"
ord' dat = [instance_context ["Ord"] "Ord" dat [FunD (mkName "compare") [(Clause
    [(VarP (lName "a")),(VarP (lName "b"))] (NormalB (applyWith (VarE (lName
    "check")) [(VarE (lName "a")),(VarE (lName "b"))])) (FunD (lName "check"
    ) ((map (\ (_,ctor) -> (Clause [(ConP (mkName (ctorName ctor)) ((map (
    \field -> (VarP (lName ("x" ++ show field)))) (id [1..ctorArity ctor]))++[
    ])),(ConP (mkName (ctorName ctor)) ((map (\field -> (VarP (lName ("y" ++
    show field)))) (id [1..ctorArity ctor]))++[]))] (NormalB (applyWith (VarE (
    mkName "compare")) [(TupE ((map (\field -> (VarE (lName ("x" ++ show field
    )))) (id [1..ctorArity ctor]))++[])),(TupE ((map (\field -> (VarE (lName (
    "y" ++ show field)))) (id [1..ctorArity ctor]))++[]))])) [])) (id (zip [0..
    ] (dataCtors dat))))
    ++ emptyIfOneCtor
    [(Clause [(VarP (lName "x")),(VarP (lName "y"))] (
    NormalB (applyWith (VarE (mkName "compare")) [(AppE (VarE (lName "tag")) (
    VarE (lName "x"))),(AppE (VarE (lName "tag")) (VarE (lName "y")))])) [])
    ])
    : emptyIfOneCtor
    [FunD (lName "tag") ((map (\ (ctorInd,ctor) -> (Clause [((flip RecP [
    ]) (mkName (ctorName ctor)))] (NormalB (LitE (IntegerL ctorInd))) [])) (id
    (zip [0..] (dataCtors dat)))))] ))]]]
  where lName = mkName . (++ "_Data_Derive_Ord__")
        emptyIfOneCtor = (guard (length (dataCtors dat) > 1) >>)

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
