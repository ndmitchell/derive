{-# OPTIONS_GHC -fth -cpp #-}

module Data.Derive.BinaryDefer(makeBinaryDefer) where

import Language.Haskell.TH.All hiding (unit)

#ifdef GUESS

import Data.DeriveGuess
import Data.Binary.Defer

instance Eq (DataName a) where

example = [d|

    instance BinaryDefer a => BinaryDefer (DataName a) where
        bothDefer = defer [\ ~(CtorZero) -> unit CtorZero
                          ,\ ~(CtorOne x1) -> unit CtorOne << x1
                          ,\ ~(CtorTwo x1 x2) -> unit CtorTwo << x1 << x2
                          ,\ ~(CtorTwo' x1 x2) -> unit CtorTwo' << x1 << x2
                          ]

    |]

#endif


makeBinaryDefer = Derivation binarydefer' "BinaryDefer"
binarydefer' dat = instance_context ["BinaryDefer"] "BinaryDefer" dat [ValD (
    VarP (mkName "bothDefer")) (NormalB (AppE (VarE (mkName "defer")) (ListE ((
    map (\(ctorInd,ctor) -> (LamE [(TildeP (ConP (mkName (ctorName ctor)) ((map
    (\field -> (VarP (mkName ("x" ++ show field)))) (id [1..ctorArity ctor]))++
    [])))] (foldr1With (VarE (mkName "<<")) ((map (\field -> (VarE (mkName ("x"
    ++ show field)))) (reverse [1..ctorArity ctor]))++[(AppE (VarE (mkName
    "unit")) (ConE (mkName (ctorName ctor))))]++[])))) (id (zip [0..] (
    dataCtors dat))))++[])))) []]
    

{-    

derive dat = simple_instance "BinaryDefer" dat [funN "bothDefer" [ body ] ]
    where
        body = sclause [] (l1 "defer" (lst [ f ct | ct <- dataCtors dat ]))

        f ctor = LamE [TildeP (ctp ctor 'v')] $
                 foldl (l2 "<<") (l1 "unit" (ctc ctor)) (ctv ctor 'v')
-}
