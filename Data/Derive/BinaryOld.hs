-- NOTE: Cannot be guessed as is not inductive because of getWordN

-- | Derivation for Data.Binary's Binary class for serializing values.
-- The generated instances implement a very simple tagged data format.
{-# OPTIONS_GHC -fth -cpp #-}

module Data.Derive.BinaryOld(makeBinaryOld) where

import Language.Haskell.TH.All
import Data.List

#ifdef GUESS

import Data.DeriveGuess
import Yhc.Core.Internal.Binary

example = (,) "BinaryOld" [d|

    instance Binary a => Binary (DataName a) where
        put_ bh x = 
            case x of
                CtorZero -> do
                    if useTag then putByte bh 0 else return ()
                CtorOne x1 -> do
                    if useTag then putByte bh 1 else return ()
                    put_ bh x1
                CtorTwo x1 x2 -> do
                    if useTag then putByte bh 2 else return ()
                    put_ bh x1
                    put_ bh x2
                CtorTwo' x1 x2 -> do
                    if useTag then putByte bh 3 else return ()
                    put_ bh x1
                    put_ bh x2
            where
                useTag = length [CtorZero{}, CtorOne{}, CtorTwo{}, CtorTwo'{}] > 1

        get bh = do
            h <- if useTag then getByte bh else 0
            case h of
                0 -> do
                    return CtorZero
                1 -> do
                    x1 <- get bh
                    return (CtorOne x1)
                2 -> do
                    x1 <- get bh
                    x2 <- get bh
                    return (CtorTwo x1 x2)
                3 -> do
                    x1 <- get bh
                    x2 <- get bh
                    return (CtorTwo' x1 x2)
                _ -> fail "invalid binary data found"
            where
                useTag = length [CtorZero{}, CtorOne{}, CtorTwo{}, CtorTwo'{}] > 1
    |]

#endif


makeBinaryOld :: Derivation
makeBinaryOld = derivation binaryOld' "BinaryOld"
binaryOld' dat = [InstanceD (concat ([(map (\tdat -> (AppT (ConT (mkName
    "Binary")) tdat)) (dataVars dat))])) (head [(AppT (ConT (mkName "Binary"))
    (lK (dataName dat) (dataVars dat)))])[(FunD (mkName "put_") [(Clause [(VarP
    (mkName "bh")),(VarP (mkName "x"))] (NormalB (CaseE (VarE (mkName "x")) ((
    map (\(ctorInd,ctor) -> (Match (ConP (mkName ("" ++ ctorName ctor)) ((map (
    \field -> (VarP (mkName ("x" ++ show field)))) (id [1..ctorArity ctor]))++[
    ])) (NormalB (DoE ([(NoBindS (CondE (VarE (mkName "useTag")) (applyWith (
    VarE (mkName "putByte")) [(VarE (mkName "bh")),(LitE (IntegerL ctorInd))])
    (AppE (VarE (mkName "return")) (TupE []))))]++(map (\field -> (NoBindS (
    applyWith (VarE (mkName "put_")) [(VarE (mkName "bh")),(VarE (mkName ("x"
    ++ show field)))]))) (id [1..ctorArity ctor]))++[]))) [])) (id (zip [0..] (
    dataCtors dat))))++[]))) [(ValD (VarP (mkName "useTag")) (NormalB (
    applyWith (VarE (mkName ">")) [(AppE (VarE (mkName "length")) (ListE ((map
    (\(ctorInd,ctor) -> ((flip RecConE []) (mkName ("" ++ ctorName ctor)))) (id
    (zip [0..] (dataCtors dat))))++[]))),(LitE (IntegerL 1))])) [])])]),(FunD (
    mkName "get") [(Clause [(VarP (mkName "bh"))] (NormalB (DoE [(BindS (VarP (
    mkName "h")) (CondE (VarE (mkName "useTag")) (AppE (VarE (mkName "getByte")
    ) (VarE (mkName "bh"))) (LitE (IntegerL 0)))),(NoBindS (CaseE (VarE (mkName
    "h")) ((map (\(ctorInd,ctor) -> (Match (LitP (IntegerL ctorInd)) (NormalB (
    DoE ((map (\field -> (BindS (VarP (mkName ("x" ++ show field))) (AppE (VarE
    (mkName "get")) (VarE (mkName "bh"))))) (id [1..ctorArity ctor]))++[(
    NoBindS (AppE (VarE (mkName "return")) (applyWith (ConE (mkName ("" ++
    ctorName ctor))) ((map (\field -> (VarE (mkName ("x" ++ show field)))) (id
    [1..ctorArity ctor]))++[]))))]++[]))) [])) (id (zip [0..] (dataCtors dat)))
    )++[(Match WildP (NormalB (AppE (VarE (mkName "fail")) (LitE (StringL
    "invalid binary data found")))) [])]++[])))])) [(ValD (VarP (mkName
    "useTag")) (NormalB (applyWith (VarE (mkName ">")) [(AppE (VarE (mkName
    "length")) (ListE ((map (\(ctorInd,ctor) -> ((flip RecConE []) (mkName (""
    ++ ctorName ctor)))) (id (zip [0..] (dataCtors dat))))++[]))),(LitE (
    IntegerL 1))])) [])])])]]
