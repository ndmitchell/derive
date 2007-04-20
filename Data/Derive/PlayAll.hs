{-# OPTIONS_GHC -fth -cpp -fglasgow-exts -fallow-undecidable-instances #-}

module Data.Derive.PlayAll(makePlayAll) where

import Language.Haskell.TH.All


#ifdef GUESS

import Data.Generics.PlayTypeable
import Data.DeriveGuess
import Data.Typeable

example = (,) "PlayAll" [d|

    instance (PlayAll a (DataName a), Typeable a) => Play (DataName a) where
        replaceChildren = replaceChildrenAll

    instance (Typeable t, Typeable a, Play t, PlayAll a t) => PlayAll (DataName a) t where
        playAll CtorZero = play CtorZero
        playAll (CtorOne x1) = play CtorOne |+ x1
        playAll (CtorTwo x1 x2) = play CtorTwo |+ x1 |+ x2
        playAll (CtorTwo' x1 x2) = play CtorTwo' |+ x1 |+ x2

    |]

#endif


makePlayAll :: Derivation
makePlayAll = Derivation playAll' "PlayAll"
playAll' dat = [InstanceD (concat ([(map (\tdat -> (AppT (AppT (ConT (mkName
    "PlayAll")) tdat) (lK (dataName dat) (dataVars dat)))) (dataVars dat)),(map
    (\tdat -> (AppT (ConT (mkName "Typeable")) tdat)) (dataVars dat))])) (head
    [(AppT (ConT (mkName "Play")) (lK (dataName dat) (dataVars dat)))])[(ValD (
    VarP (mkName "replaceChildren")) (NormalB (VarE (mkName
    "replaceChildrenAll"))) [])],InstanceD (concat ([[(AppT (ConT (mkName
    "Typeable")) (VarT (mkName "t")))],(map (\tdat -> (AppT (ConT (mkName
    "Typeable")) tdat)) (dataVars dat)),[(AppT (ConT (mkName "Play")) (VarT (
    mkName "t")))],(map (\tdat -> (AppT (AppT (ConT (mkName "PlayAll")) tdat) (
    VarT (mkName "t")))) (dataVars dat))])) (head [(AppT (AppT (ConT (mkName
    "PlayAll")) (lK (dataName dat) (dataVars dat))) (VarT (mkName "t")))])[(
    FunD (mkName "playAll") ((map (\(ctorInd,ctor) -> (Clause [(ConP (mkName (
    "" ++ ctorName ctor)) ((map (\field -> (VarP (mkName ("x" ++ show field))))
    (id [1..ctorArity ctor]))++[]))] (NormalB (foldr1With (VarE (mkName "|+"))
    ((map (\field -> (VarE (mkName ("x" ++ show field)))) (reverse [1..
    ctorArity ctor]))++[(AppE (VarE (mkName "play")) (ConE (mkName ("" ++
    ctorName ctor))))]++[]))) [])) (id (zip [0..] (dataCtors dat))))++[]))]]
