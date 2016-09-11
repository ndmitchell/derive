module Data.Derive.Enum where
{-
import Prelude

example :: Sample

instance Enum (Sample a) where
    toEnum 0 = First{}
    toEnum 1 = Second{}
    toEnum 2 = Third{}
    toEnum n = error $ "toEnum " ++ show n ++ ", not defined for " ++ "Sample"

    fromEnum (First{}) = 0
    fromEnum (Second{}) = 1
    fromEnum (Third{}) = 2

-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeEnum :: Derivation
makeEnum = derivationDSL "Enum" dslEnum

dslEnum =
    List [Instance [] "Enum" (App "Just" (List [List [App "InsDecl" (
    List [App "()" (List []),App "FunBind" (List [App "()" (List []),
    Concat (List [MapCtor (App "Match" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "toEnum"]),List [App
    "PLit" (List [App "()" (List []),App "Signless" (List [App "()" (
    List [])]),App "Int" (List [App "()" (List []),CtorIndex,ShowInt
    CtorIndex])])],App "UnGuardedRhs" (List [App "()" (List []),App
    "RecConstr" (List [App "()" (List []),App "UnQual" (List [App "()"
    (List []),App "Ident" (List [App "()" (List []),CtorName])]),List
    []])]),App "Nothing" (List [])])),List [App "Match" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),String
    "toEnum"]),List [App "PVar" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),String "n"])])],App "UnGuardedRhs" (List
    [App "()" (List []),App "InfixApp" (List [App "()" (List []),App
    "Var" (List [App "()" (List []),App "UnQual" (List [App "()" (List
    []),App "Ident" (List [App "()" (List []),String "error"])])]),App
    "QVarOp" (List [App "()" (List []),App "UnQual" (List [App "()" (
    List []),App "Symbol" (List [App "()" (List []),String "$"])])]),
    Fold (App "InfixApp" (List [App "()" (List []),Head,App "QVarOp" (
    List [App "()" (List []),App "UnQual" (List [App "()" (List []),
    App "Symbol" (List [App "()" (List []),String "++"])])]),Tail])) (
    List [App "Lit" (List [App "()" (List []),App "String" (List [App
    "()" (List []),String "toEnum ",String "toEnum "])]),App "App" (
    List [App "()" (List []),App "Var" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "show"])])]),App "Var" (List [App "()" (List []),
    App "UnQual" (List [App "()" (List []),App "Ident" (List [App "()"
    (List []),String "n"])])])]),App "Lit" (List [App "()" (List []),
    App "String" (List [App "()" (List []),String ", not defined for "
    ,String ", not defined for "])]),App "Lit" (List [App "()" (List [
    ]),App "String" (List [App "()" (List []),DataName,DataName])])])]
    )]),App "Nothing" (List [])])]])])]),App "InsDecl" (List [App "()"
    (List []),App "FunBind" (List [App "()" (List []),MapCtor (App
    "Match" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "fromEnum"]),List [App "PParen" (List [App "()" (
    List []),App "PRec" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Ident" (List [App "()" (List []),CtorName]
    )]),List []])])],App "UnGuardedRhs" (List [App "()" (List []),App
    "Lit" (List [App "()" (List []),App "Int" (List [App "()" (List []
    ),CtorIndex,ShowInt CtorIndex])])]),App "Nothing" (List [])]))])])
    ]]))]
-- GENERATED STOP
