module Data.Derive.BinaryDefer where
{-
import "binarydefer" Data.Binary.Defer

example :: Sample

instance BinaryDefer a => BinaryDefer (Sample a) where
    bothDefer = defer [\ ~(o@(First)) -> if null [] then unit (First) <<! o else unit First
                      ,\ ~(o@(Second x1 x2)) -> if null [const () x1, const () x2] then unit (Second x1 x2) <<! o else unit Second << x1 << x2
                      ,\ ~(o@(Third x1)) -> if null [const () x1] then unit (Third x1) <<! o else unit Third << x1
                      ]

test :: FailList

instance (BinaryDefer e, BinaryDefer a) => BinaryDefer (FailList e a) where
    bothDefer = defer [\ ~(o@Zoro) -> unit Zoro <<! o
                      ,\ ~(Fial x1) -> unit Fial << x1
                      ,\ ~(Const x1 x2) -> unit Const << x1 << x2
                      ]

-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeBinaryDefer :: Derivation
makeBinaryDefer = derivationDSL "BinaryDefer" dslBinaryDefer

dslBinaryDefer =
    List [Instance ["BinaryDefer"] "BinaryDefer" (List [App "InsDecl"
    (List [App "PatBind" (List [App "PVar" (List [App "Ident" (List [
    String "bothDefer"])]),App "Nothing" (List []),App "UnGuardedRhs"
    (List [App "App" (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "defer"])])]),App "List" (List [MapCtor (App
    "Lambda" (List [List [App "PIrrPat" (List [App "PParen" (List [App
    "PAsPat" (List [App "Ident" (List [String "o"]),App "PParen" (List
    [App "PApp" (List [App "UnQual" (List [App "Ident" (List [CtorName
    ])]),MapField (App "PVar" (List [App "Ident" (List [Concat (List [
    String "x",ShowInt FieldIndex])])]))])])])])])],App "If" (List [
    App "App" (List [App "Var" (List [App "UnQual" (List [App "Ident"
    (List [String "null"])])]),App "List" (List [MapField (Application
    (List [App "Var" (List [App "UnQual" (List [App "Ident" (List [
    String "const"])])]),App "Con" (List [App "Special" (List [App
    "UnitCon" (List [])])]),App "Var" (List [App "UnQual" (List [App
    "Ident" (List [Concat (List [String "x",ShowInt FieldIndex])])])])
    ]))])]),App "InfixApp" (List [App "App" (List [App "Var" (List [
    App "UnQual" (List [App "Ident" (List [String "unit"])])]),App
    "Paren" (List [Application (Concat (List [List [App "Con" (List [
    App "UnQual" (List [App "Ident" (List [CtorName])])])],MapField (
    App "Var" (List [App "UnQual" (List [App "Ident" (List [Concat (
    List [String "x",ShowInt FieldIndex])])])]))]))])]),App "QVarOp" (
    List [App "UnQual" (List [App "Symbol" (List [String "<<!"])])]),
    App "Var" (List [App "UnQual" (List [App "Ident" (List [String "o"
    ])])])]),Fold (App "InfixApp" (List [Tail,App "QVarOp" (List [App
    "UnQual" (List [App "Symbol" (List [String "<<"])])]),Head])) (
    Concat (List [Reverse (MapField (App "Var" (List [App "UnQual" (
    List [App "Ident" (List [Concat (List [String "x",ShowInt
    FieldIndex])])])]))),List [App "App" (List [App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "unit"])])]),App "Con" (
    List [App "UnQual" (List [App "Ident" (List [CtorName])])])])]]))]
    )]))])])]),App "BDecls" (List [List []])])])])]
-- GENERATED STOP
