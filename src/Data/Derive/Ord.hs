module Data.Derive.Ord where
{-
import Prelude

example :: Sample
instance Ord a => Ord (Sample a) where
    compare a b = check a b
        where
            check (First) (First) = EQ
            check (Second x1 x2) (Second y1 y2) = compare x1 y1 `_then` compare x2 y2 `_then` EQ
            check (Third x1) (Third y1) = compare x1 y1 `_then` EQ
            check x y | length [First{},Second{},Third{}] > 1 = compare (tag x) (tag y)

            -- leading _ to avoid a warning when not used
            EQ `_then` x = x
            x `_then` _ = x

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
    List [Instance ["Ord"] "Ord" (App "Just" (List [List [App
    "InsDecl" (List [App "()" (List []),App "FunBind" (List [App "()"
    (List []),List [App "Match" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),String "compare"]),List [App "PVar" (
    List [App "()" (List []),App "Ident" (List [App "()" (List []),
    String "a"])]),App "PVar" (List [App "()" (List []),App "Ident" (
    List [App "()" (List []),String "b"])])],App "UnGuardedRhs" (List
    [App "()" (List []),Application (List [App "Var" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "check"])])]),App "Var" (List [App "()"
    (List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "a"])])]),App "Var" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "b"])])])])]),App "Just" (List [App
    "BDecls" (List [App "()" (List []),List [App "FunBind" (List [App
    "()" (List []),Concat (List [MapCtor (App "Match" (List [App "()"
    (List []),App "Ident" (List [App "()" (List []),String "check"]),
    List [App "PParen" (List [App "()" (List []),App "PApp" (List [App
    "()" (List []),App "UnQual" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),CtorName])]),MapField (App "PVar" (List
    [App "()" (List []),App "Ident" (List [App "()" (List []),Concat (
    List [String "x",ShowInt FieldIndex])])]))])]),App "PParen" (List
    [App "()" (List []),App "PApp" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),CtorName])]),MapField (App "PVar" (List [App "()" (List [
    ]),App "Ident" (List [App "()" (List []),Concat (List [String "y",
    ShowInt FieldIndex])])]))])])],App "UnGuardedRhs" (List [App "()"
    (List []),Fold (App "InfixApp" (List [App "()" (List []),Tail,App
    "QVarOp" (List [App "()" (List []),App "UnQual" (List [App "()" (
    List []),App "Ident" (List [App "()" (List []),String "_then"])])]
    ),Head])) (Concat (List [List [App "Con" (List [App "()" (List [])
    ,App "UnQual" (List [App "()" (List []),App "Ident" (List [App
    "()" (List []),String "EQ"])])])],Reverse (MapField (Application (
    List [App "Var" (List [App "()" (List []),App "UnQual" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),String
    "compare"])])]),App "Var" (List [App "()" (List []),App "UnQual" (
    List [App "()" (List []),App "Ident" (List [App "()" (List []),
    Concat (List [String "x",ShowInt FieldIndex])])])]),App "Var" (
    List [App "()" (List []),App "UnQual" (List [App "()" (List []),
    App "Ident" (List [App "()" (List []),Concat (List [String "y",
    ShowInt FieldIndex])])])])])))]))]),App "Nothing" (List [])])),
    List [App "Match" (List [App "()" (List []),App "Ident" (List [App
    "()" (List []),String "check"]),List [App "PVar" (List [App "()" (
    List []),App "Ident" (List [App "()" (List []),String "x"])]),App
    "PVar" (List [App "()" (List []),App "Ident" (List [App "()" (List
    []),String "y"])])],App "GuardedRhss" (List [App "()" (List []),
    List [App "GuardedRhs" (List [App "()" (List []),List [App
    "Qualifier" (List [App "()" (List []),App "InfixApp" (List [App
    "()" (List []),App "App" (List [App "()" (List []),App "Var" (List
    [App "()" (List []),App "UnQual" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "length"])])]),App "List"
    (List [App "()" (List []),MapCtor (App "RecConstr" (List [App "()"
    (List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),CtorName])]),List []]))])]),App "QVarOp" (List
    [App "()" (List []),App "UnQual" (List [App "()" (List []),App
    "Symbol" (List [App "()" (List []),String ">"])])]),App "Lit" (
    List [App "()" (List []),App "Int" (List [App "()" (List []),Int 1
    ,ShowInt (Int 1)])])])])],Application (List [App "Var" (List [App
    "()" (List []),App "UnQual" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),String "compare"])])]),App "Paren" (List
    [App "()" (List []),App "App" (List [App "()" (List []),App "Var"
    (List [App "()" (List []),App "UnQual" (List [App "()" (List []),
    App "Ident" (List [App "()" (List []),String "tag"])])]),App "Var"
    (List [App "()" (List []),App "UnQual" (List [App "()" (List []),
    App "Ident" (List [App "()" (List []),String "x"])])])])]),App
    "Paren" (List [App "()" (List []),App "App" (List [App "()" (List
    []),App "Var" (List [App "()" (List []),App "UnQual" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),String "tag"]
    )])]),App "Var" (List [App "()" (List []),App "UnQual" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),String "y"])]
    )])])])])])]]),App "Nothing" (List [])])]])]),App "FunBind" (List
    [App "()" (List []),List [App "InfixMatch" (List [App "()" (List [
    ]),App "PApp" (List [App "()" (List []),App "UnQual" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),String "EQ"])
    ]),List []]),App "Ident" (List [App "()" (List []),String "_then"]
    ),List [App "PVar" (List [App "()" (List []),App "Ident" (List [
    App "()" (List []),String "x"])])],App "UnGuardedRhs" (List [App
    "()" (List []),App "Var" (List [App "()" (List []),App "UnQual" (
    List [App "()" (List []),App "Ident" (List [App "()" (List []),
    String "x"])])])]),App "Nothing" (List [])]),App "InfixMatch" (
    List [App "()" (List []),App "PVar" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "x"])]),App "Ident" (List
    [App "()" (List []),String "_then"]),List [App "PWildCard" (List [
    App "()" (List [])])],App "UnGuardedRhs" (List [App "()" (List [])
    ,App "Var" (List [App "()" (List []),App "UnQual" (List [App "()"
    (List []),App "Ident" (List [App "()" (List []),String "x"])])])])
    ,App "Nothing" (List [])])]]),App "FunBind" (List [App "()" (List
    []),MapCtor (App "Match" (List [App "()" (List []),App "Ident" (
    List [App "()" (List []),String "tag"]),List [App "PParen" (List [
    App "()" (List []),App "PRec" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),CtorName])]),List []])])],App "UnGuardedRhs" (List [App
    "()" (List []),App "ExpTypeSig" (List [App "()" (List []),App
    "Lit" (List [App "()" (List []),App "Int" (List [App "()" (List []
    ),CtorIndex,ShowInt CtorIndex])]),App "TyCon" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "Int"])])])])]),App "Nothing" (List [])
    ]))])]])])])]])])]]))]
-- GENERATED STOP
