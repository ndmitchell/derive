{-# OPTIONS_GHC -fth -cpp -fglasgow-exts -fallow-undecidable-instances #-}

module Data.Derive.PlateTypeable(makePlateTypeable) where

import Language.Haskell.TH.All


#ifdef GUESS

import Data.Generics.PlateTypeable
import Data.DeriveGuess
import Data.Typeable

example = (,) "PlateTypeable" [d|

    instance (PlateAll a (DataName a), Typeable a) => Uniplate (DataName a) where
        replaceChildren = replaceChildrenAll

    instance (Typeable t, Typeable a, Uniplate t, PlateAll a t) => PlateAll (DataName a) t where
        plateAll CtorZero         = plate CtorZero
        plateAll (CtorOne x1)     = plate CtorOne  |+ x1
        plateAll (CtorTwo x1 x2)  = plate CtorTwo  |+ x1 |+ x2
        plateAll (CtorTwo' x1 x2) = plate CtorTwo' |+ x1 |+ x2

    |]

#endif


makePlateTypeable :: Derivation
makePlateTypeable = Derivation plateTypeable' "PlateTypeable"
plateTypeable' dat = [InstanceD (concat ([(map (\tdat -> (AppT (AppT (ConT (
    mkName "PlateAll")) tdat) (lK (dataName dat) (dataVars dat)))) (dataVars
    dat)),(map (\tdat -> (AppT (ConT (mkName "Typeable")) tdat)) (dataVars dat)
    )])) (head [(AppT (ConT (mkName "Uniplate")) (lK (dataName dat) (dataVars
    dat)))])[(ValD (VarP (mkName "replaceChildren")) (NormalB (VarE (mkName
    "replaceChildrenAll"))) [])],InstanceD (concat ([[(AppT (ConT (mkName
    "Typeable")) (VarT (mkName "t")))],(map (\tdat -> (AppT (ConT (mkName
    "Typeable")) tdat)) (dataVars dat)),[(AppT (ConT (mkName "Uniplate")) (VarT
    (mkName "t")))],(map (\tdat -> (AppT (AppT (ConT (mkName "PlateAll")) tdat)
    (VarT (mkName "t")))) (dataVars dat))])) (head [(AppT (AppT (ConT (mkName
    "PlateAll")) (lK (dataName dat) (dataVars dat))) (VarT (mkName "t")))])[(
    FunD (mkName "plateAll") ((map (\(ctorInd,ctor) -> (Clause [(ConP (mkName (
    "" ++ ctorName ctor)) ((map (\field -> (VarP (mkName ("x" ++ show field))))
    (id [1..ctorArity ctor]))++[]))] (NormalB (foldr1With (VarE (mkName "|+"))
    ((map (\field -> (VarE (mkName ("x" ++ show field)))) (reverse [1..
    ctorArity ctor]))++[(AppE (VarE (mkName "plate")) (ConE (mkName ("" ++
    ctorName ctor))))]++[]))) [])) (id (zip [0..] (dataCtors dat))))++[]))]]
