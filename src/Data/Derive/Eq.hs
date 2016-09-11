module Data.Derive.Eq where
{-
import Prelude

example :: Sample

instance (Eq a) => Eq (Sample a) where
    First == First = True
    Second x1 x2 == Second y1 y2 = x1 == y1 && x2 == y2 && True
    Third x1 == Third y1 = x1 == y1 && True
    _ == _ | length [First{},Second{},Third{}] > 1 = False

test :: Assoced
instance Eq typ => Eq (Assoced typ) where
    Assoced x1 x2 == Assoced y1 y2 = x1 == y1 && x2 == y2

test :: TwoParam
instance Eq b => Eq (TwoParam a b) where
    TwoParam x1 == TwoParam y1 = x1 == y1

-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeEq :: Derivation
makeEq = derivationDSL "Eq" dslEq

dslEq =
    List [Instance [] "Eq" (App "Just" (List [List [App "InsDecl" (
    List [App "()" (List []),App "FunBind" (List [App "()" (List []),
    Concat (List [MapCtor (App "InfixMatch" (List [App "()" (List []),
    App "PApp" (List [App "()" (List []),App "UnQual" (List [App "()"
    (List []),App "Ident" (List [App "()" (List []),CtorName])]),
    MapField (App "PVar" (List [App "()" (List []),App "Ident" (List [
    App "()" (List []),Concat (List [String "x",ShowInt FieldIndex])])
    ]))]),App "Symbol" (List [App "()" (List []),String "=="]),List [
    App "PApp" (List [App "()" (List []),App "UnQual" (List [App "()"
    (List []),App "Ident" (List [App "()" (List []),CtorName])]),
    MapField (App "PVar" (List [App "()" (List []),App "Ident" (List [
    App "()" (List []),Concat (List [String "y",ShowInt FieldIndex])])
    ]))])],App "UnGuardedRhs" (List [App "()" (List []),Fold (App
    "InfixApp" (List [App "()" (List []),Head,App "QVarOp" (List [App
    "()" (List []),App "UnQual" (List [App "()" (List []),App "Symbol"
    (List [App "()" (List []),String "&&"])])]),Tail])) (Concat (List
    [MapField (App "InfixApp" (List [App "()" (List []),App "Var" (
    List [App "()" (List []),App "UnQual" (List [App "()" (List []),
    App "Ident" (List [App "()" (List []),Concat (List [String "x",
    ShowInt FieldIndex])])])]),App "QVarOp" (List [App "()" (List []),
    App "UnQual" (List [App "()" (List []),App "Symbol" (List [App
    "()" (List []),String "=="])])]),App "Var" (List [App "()" (List [
    ]),App "UnQual" (List [App "()" (List []),App "Ident" (List [App
    "()" (List []),Concat (List [String "y",ShowInt FieldIndex])])])])
    ])),List [App "Con" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Ident" (List [App "()" (List []),String
    "True"])])])]]))]),App "Nothing" (List [])])),List [App
    "InfixMatch" (List [App "()" (List []),App "PWildCard" (List [App
    "()" (List [])]),App "Symbol" (List [App "()" (List []),String
    "=="]),List [App "PWildCard" (List [App "()" (List [])])],App
    "GuardedRhss" (List [App "()" (List []),List [App "GuardedRhs" (
    List [App "()" (List []),List [App "Qualifier" (List [App "()" (
    List []),App "InfixApp" (List [App "()" (List []),App "App" (List
    [App "()" (List []),App "Var" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "length"])])]),App "List" (List [App "()" (List []
    ),MapCtor (App "RecConstr" (List [App "()" (List []),App "UnQual"
    (List [App "()" (List []),App "Ident" (List [App "()" (List []),
    CtorName])]),List []]))])]),App "QVarOp" (List [App "()" (List [])
    ,App "UnQual" (List [App "()" (List []),App "Symbol" (List [App
    "()" (List []),String ">"])])]),App "Lit" (List [App "()" (List []
    ),App "Int" (List [App "()" (List []),Int 1,ShowInt (Int 1)])])])]
    )],App "Con" (List [App "()" (List []),App "UnQual" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),String
    "False"])])])])]]),App "Nothing" (List [])])]])])])]]))]
-- GENERATED STOP
