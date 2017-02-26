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
    List [Instance ["BinaryDefer"] "BinaryDefer" (App "Just" (List [
    List [App "InsDecl" (List [App "()" (List []),App "PatBind" (List
    [App "()" (List []),App "PVar" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "bothDefer"])]),App
    "UnGuardedRhs" (List [App "()" (List []),App "App" (List [App "()"
    (List []),App "Var" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Ident" (List [App "()" (List []),String
    "defer"])])]),App "List" (List [App "()" (List []),MapCtor (App
    "Lambda" (List [App "()" (List []),List [App "PIrrPat" (List [App
    "()" (List []),App "PParen" (List [App "()" (List []),App "PAsPat"
    (List [App "()" (List []),App "Ident" (List [App "()" (List []),
    String "o"]),App "PParen" (List [App "()" (List []),App "PApp" (
    List [App "()" (List []),App "UnQual" (List [App "()" (List []),
    App "Ident" (List [App "()" (List []),CtorName])]),MapField (App
    "PVar" (List [App "()" (List []),App "Ident" (List [App "()" (List
    []),Concat (List [String "x",ShowInt FieldIndex])])]))])])])])])],
    App "If" (List [App "()" (List []),App "App" (List [App "()" (List
    []),App "Var" (List [App "()" (List []),App "UnQual" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),String "null"
    ])])]),App "List" (List [App "()" (List []),MapField (Application
    (List [App "Var" (List [App "()" (List []),App "UnQual" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),String
    "const"])])]),App "Con" (List [App "()" (List []),App "Special" (
    List [App "()" (List []),App "UnitCon" (List [App "()" (List [])])
    ])]),App "Var" (List [App "()" (List []),App "UnQual" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),Concat (List
    [String "x",ShowInt FieldIndex])])])])]))])]),App "InfixApp" (List
    [App "()" (List []),App "App" (List [App "()" (List []),App "Var"
    (List [App "()" (List []),App "UnQual" (List [App "()" (List []),
    App "Ident" (List [App "()" (List []),String "unit"])])]),App
    "Paren" (List [App "()" (List []),Application (Concat (List [List
    [App "Con" (List [App "()" (List []),App "UnQual" (List [App "()"
    (List []),App "Ident" (List [App "()" (List []),CtorName])])])],
    MapField (App "Var" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Ident" (List [App "()" (List []),Concat (
    List [String "x",ShowInt FieldIndex])])])]))]))])]),App "QVarOp" (
    List [App "()" (List []),App "UnQual" (List [App "()" (List []),
    App "Symbol" (List [App "()" (List []),String "<<!"])])]),App
    "Var" (List [App "()" (List []),App "UnQual" (List [App "()" (List
    []),App "Ident" (List [App "()" (List []),String "o"])])])]),Fold
    (App "InfixApp" (List [App "()" (List []),Tail,App "QVarOp" (List
    [App "()" (List []),App "UnQual" (List [App "()" (List []),App
    "Symbol" (List [App "()" (List []),String "<<"])])]),Head])) (
    Concat (List [Reverse (MapField (App "Var" (List [App "()" (List [
    ]),App "UnQual" (List [App "()" (List []),App "Ident" (List [App
    "()" (List []),Concat (List [String "x",ShowInt FieldIndex])])])])
    )),List [App "App" (List [App "()" (List []),App "Var" (List [App
    "()" (List []),App "UnQual" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),String "unit"])])]),App "Con" (List [App
    "()" (List []),App "UnQual" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),CtorName])])])])]]))])]))])])]),App
    "Nothing" (List [])])])]]))]
-- GENERATED STOP
