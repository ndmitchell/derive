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
    List [Instance ["Eq"] "Eq" (List [App "InsDecl" (List [App
    "FunBind" (List [Concat (List [MapCtor (App "Match" (List [App
    "Symbol" (List [String "=="]),List [App "PApp" (List [App "UnQual"
    (List [App "Ident" (List [CtorName])]),MapField (App "PVar" (List
    [App "Ident" (List [Concat (List [String "x",ShowInt FieldIndex])]
    )]))]),App "PApp" (List [App "UnQual" (List [App "Ident" (List [
    CtorName])]),MapField (App "PVar" (List [App "Ident" (List [Concat
    (List [String "y",ShowInt FieldIndex])])]))])],App "Nothing" (List
    []),App "UnGuardedRhs" (List [Fold (App "InfixApp" (List [Head,App
    "QVarOp" (List [App "UnQual" (List [App "Symbol" (List [String
    "&&"])])]),Tail])) (Concat (List [MapField (App "InfixApp" (List [
    App "Var" (List [App "UnQual" (List [App "Ident" (List [Concat (
    List [String "x",ShowInt FieldIndex])])])]),App "QVarOp" (List [
    App "UnQual" (List [App "Symbol" (List [String "=="])])]),App
    "Var" (List [App "UnQual" (List [App "Ident" (List [Concat (List [
    String "y",ShowInt FieldIndex])])])])])),List [App "Con" (List [
    App "UnQual" (List [App "Ident" (List [String "True"])])])]]))]),
    App "Nothing" (List [])])),List [App "Match" (List [App "Symbol" (
    List [String "=="]),List [App "PWildCard" (List []),App
    "PWildCard" (List [])],App "Nothing" (List []),App "GuardedRhss" (
    List [List [App "GuardedRhs" (List [List [App "Qualifier" (List [
    App "InfixApp" (List [App "App" (List [App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "length"])])]),App
    "List" (List [MapCtor (App "RecConstr" (List [App "UnQual" (List [
    App "Ident" (List [CtorName])]),List []]))])]),App "QVarOp" (List
    [App "UnQual" (List [App "Symbol" (List [String ">"])])]),App
    "Lit" (List [App "Int" (List [Int 1])])])])],App "Con" (List [App
    "UnQual" (List [App "Ident" (List [String "False"])])])])]]),App
    "Nothing" (List [])])]])])])])]
-- GENERATED STOP
