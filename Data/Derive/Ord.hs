module Data.Derive.Ord where
{-
import Prelude

example :: Sample
instance Ord a => Ord (Sample a) where
    compare a b = check a b
        where
            check (First) (First) = EQ
            check (Second x1 x2) (Second y1 y2) = compare x1 y1 & compare x2 y2 & EQ
            check (Third x1) (Third y1) = compare x1 y1 & EQ
            check x y | length [First{},Second{},Third{}] > 1 = compare (tag x) (tag y)

            EQ & x = x
            x & _ = x

            tag (First{}) = 0 :: Int
            tag (Second{}) = 1 :: Int
            tag (Third{}) = 2 :: Int

-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeOrd :: Derivation
makeOrd = derivationDSL "Ord" dslOrd

dslOrd =
    List [Instance ["Ord"] "Ord" (List [App "InsDecl" (List [App
    "FunBind" (List [List [App "Match" (List [App "Ident" (List [
    String "compare"]),List [App "PVar" (List [App "Ident" (List [
    String "a"])]),App "PVar" (List [App "Ident" (List [String "b"])])
    ],App "Nothing" (List []),App "UnGuardedRhs" (List [Application (
    List [App "Var" (List [App "UnQual" (List [App "Ident" (List [
    String "check"])])]),App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "a"])])]),App "Var" (List [App "UnQual" (
    List [App "Ident" (List [String "b"])])])])]),App "BDecls" (List [
    List [App "FunBind" (List [Concat (List [MapCtor (App "Match" (
    List [App "Ident" (List [String "check"]),List [App "PParen" (List
    [App "PApp" (List [App "UnQual" (List [App "Ident" (List [CtorName
    ])]),MapField (App "PVar" (List [App "Ident" (List [Concat (List [
    String "x",ShowInt FieldIndex])])]))])]),App "PParen" (List [App
    "PApp" (List [App "UnQual" (List [App "Ident" (List [CtorName])]),
    MapField (App "PVar" (List [App "Ident" (List [Concat (List [
    String "y",ShowInt FieldIndex])])]))])])],App "Nothing" (List []),
    App "UnGuardedRhs" (List [Fold (App "InfixApp" (List [Tail,App
    "QVarOp" (List [App "UnQual" (List [App "Symbol" (List [String "&"
    ])])]),Head])) (Concat (List [List [App "Con" (List [App "UnQual"
    (List [App "Ident" (List [String "EQ"])])])],Reverse (MapField (
    Application (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "compare"])])]),App "Var" (List [App
    "UnQual" (List [App "Ident" (List [Concat (List [String "x",
    ShowInt FieldIndex])])])]),App "Var" (List [App "UnQual" (List [
    App "Ident" (List [Concat (List [String "y",ShowInt FieldIndex])])
    ])])])))]))]),App "BDecls" (List [List []])])),List [App "Match" (
    List [App "Ident" (List [String "check"]),List [App "PVar" (List [
    App "Ident" (List [String "x"])]),App "PVar" (List [App "Ident" (
    List [String "y"])])],App "Nothing" (List []),App "GuardedRhss" (
    List [List [App "GuardedRhs" (List [List [App "Qualifier" (List [
    App "InfixApp" (List [App "App" (List [App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "length"])])]),App
    "List" (List [MapCtor (App "RecConstr" (List [App "UnQual" (List [
    App "Ident" (List [CtorName])]),List []]))])]),App "QVarOp" (List
    [App "UnQual" (List [App "Symbol" (List [String ">"])])]),App
    "Lit" (List [App "Int" (List [Int 1])])])])],Application (List [
    App "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "compare"])])]),App "Paren" (List [App "App" (List [App "Var" (
    List [App "UnQual" (List [App "Ident" (List [String "tag"])])]),
    App "Var" (List [App "UnQual" (List [App "Ident" (List [String "x"
    ])])])])]),App "Paren" (List [App "App" (List [App "Var" (List [
    App "UnQual" (List [App "Ident" (List [String "tag"])])]),App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String "y"])])
    ])])])])])]]),App "BDecls" (List [List []])])]])]),App "FunBind" (
    List [List [App "Match" (List [App "Symbol" (List [String "&"]),
    List [App "PApp" (List [App "UnQual" (List [App "Ident" (List [
    String "EQ"])]),List []]),App "PVar" (List [App "Ident" (List [
    String "x"])])],App "Nothing" (List []),App "UnGuardedRhs" (List [
    App "Var" (List [App "UnQual" (List [App "Ident" (List [String "x"
    ])])])]),App "BDecls" (List [List []])]),App "Match" (List [App
    "Symbol" (List [String "&"]),List [App "PVar" (List [App "Ident" (
    List [String "x"])]),App "PWildCard" (List [])],App "Nothing" (
    List []),App "UnGuardedRhs" (List [App "Var" (List [App "UnQual" (
    List [App "Ident" (List [String "x"])])])]),App "BDecls" (List [
    List []])])]]),App "FunBind" (List [MapCtor (App "Match" (List [
    App "Ident" (List [String "tag"]),List [App "PParen" (List [App
    "PRec" (List [App "UnQual" (List [App "Ident" (List [CtorName])]),
    List []])])],App "Nothing" (List []),App "UnGuardedRhs" (List [App
    "ExpTypeSig" (List [App "Lit" (List [App "Int" (List [CtorIndex])]
    ),App "TyCon" (List [App "UnQual" (List [App "Ident" (List [String
    "Int"])])])])]),App "BDecls" (List [List []])]))])]])])]])])])]
-- GENERATED STOP
