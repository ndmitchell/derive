{-
import {- "binarydefer" -} Data.Binary.Defer

{-# EXAMPLE #-}

instance BinaryDefer a => BinaryDefer (Sample a) where
    bothDefer = defer [\ ~(First) -> unit First
                      ,\ ~(Second x1 x2) -> unit Second << x1 << x2
                      ,\ ~(Third x1) -> unit Third << x1
                      ]

-}
-- GENERATED START

module Data.Derive.BinaryDefer where

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

dslBinaryDefer =
    List [Instance ["BinaryDefer"] "BinaryDefer" (List [App "InsDecl"
    (List [App "PatBind" (List [App "PVar" (List [App "Ident" (List [
    String "bothDefer"])]),App "Nothing" (List []),App "UnGuardedRhs"
    (List [App "App" (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "defer"])])]),App "List" (List [MapCtor (App
    "Lambda" (List [List [App "PIrrPat" (List [App "PParen" (List [App
    "PApp" (List [App "UnQual" (List [App "Ident" (List [CtorName])]),
    MapField (App "PVar" (List [App "Ident" (List [Concat (List [
    String "x",ShowInt FieldIndex])])]))])])])],Fold (App "InfixApp" (
    List [Tail,App "QVarOp" (List [App "UnQual" (List [App "Symbol" (
    List [String "<<"])])]),Head])) (Concat (List [Reverse (MapField (
    App "Var" (List [App "UnQual" (List [App "Ident" (List [Concat (
    List [String "x",ShowInt FieldIndex])])])]))),List [App "App" (
    List [App "Var" (List [App "UnQual" (List [App "Ident" (List [
    String "unit"])])]),App "Con" (List [App "UnQual" (List [App
    "Ident" (List [CtorName])])])])]]))]))])])]),App "BDecls" (List [
    List []])])])])]

makeBinaryDefer :: Derivation
makeBinaryDefer = derivationDSL "BinaryDefer" dslBinaryDefer

-- GENERATED STOP
