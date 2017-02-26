module Data.Derive.EnumCyclic where
{-
import Prelude(Enum)

example :: Sample

instance Enum (Sample a) where
    toEnum 0 = First{}
    toEnum 1 = Second{}
    toEnum 2 = Third{}
    toEnum n = error $ "toEnum " ++ show n ++ ", not defined for Sample"

    fromEnum (First{}) = 0
    fromEnum (Second{}) = 1
    fromEnum (Third{}) = 2


    succ a = if b == length [First{},Second{},Third{}] then toEnum 0 else toEnum (b+1)
        where b = fromEnum a

    pred a = if b == 0 then toEnum (length [First{},Second{},Third{}]) else toEnum (b-1)
        where b = fromEnum a

-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeEnumCyclic :: Derivation
makeEnumCyclic = derivationDSL "EnumCyclic" dslEnumCyclic

dslEnumCyclic =
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
    App "String" (List [App "()" (List []),Concat (List [String
    ", not defined for ",DataName]),Concat (List [String
    ", not defined for ",DataName])])])])])]),App "Nothing" (List [])]
    )]])])]),App "InsDecl" (List [App "()" (List []),App "FunBind" (
    List [App "()" (List []),MapCtor (App "Match" (List [App "()" (
    List []),App "Ident" (List [App "()" (List []),String "fromEnum"])
    ,List [App "PParen" (List [App "()" (List []),App "PRec" (List [
    App "()" (List []),App "UnQual" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),CtorName])]),List []])])],App
    "UnGuardedRhs" (List [App "()" (List []),App "Lit" (List [App "()"
    (List []),App "Int" (List [App "()" (List []),CtorIndex,ShowInt
    CtorIndex])])]),App "Nothing" (List [])]))])]),App "InsDecl" (List
    [App "()" (List []),App "FunBind" (List [App "()" (List []),List [
    App "Match" (List [App "()" (List []),App "Ident" (List [App "()"
    (List []),String "succ"]),List [App "PVar" (List [App "()" (List [
    ]),App "Ident" (List [App "()" (List []),String "a"])])],App
    "UnGuardedRhs" (List [App "()" (List []),App "If" (List [App "()"
    (List []),App "InfixApp" (List [App "()" (List []),App "Var" (List
    [App "()" (List []),App "UnQual" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "b"])])]),App "QVarOp" (
    List [App "()" (List []),App "UnQual" (List [App "()" (List []),
    App "Symbol" (List [App "()" (List []),String "=="])])]),App "App"
    (List [App "()" (List []),App "Var" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "length"])])]),App "List" (List [App "()" (List []
    ),MapCtor (App "RecConstr" (List [App "()" (List []),App "UnQual"
    (List [App "()" (List []),App "Ident" (List [App "()" (List []),
    CtorName])]),List []]))])])]),App "App" (List [App "()" (List []),
    App "Var" (List [App "()" (List []),App "UnQual" (List [App "()" (
    List []),App "Ident" (List [App "()" (List []),String "toEnum"])])
    ]),App "Lit" (List [App "()" (List []),App "Int" (List [App "()" (
    List []),Int 0,ShowInt (Int 0)])])]),App "App" (List [App "()" (
    List []),App "Var" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Ident" (List [App "()" (List []),String
    "toEnum"])])]),App "Paren" (List [App "()" (List []),App
    "InfixApp" (List [App "()" (List []),App "Var" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "b"])])]),App "QVarOp" (List [App "()"
    (List []),App "UnQual" (List [App "()" (List []),App "Symbol" (
    List [App "()" (List []),String "+"])])]),App "Lit" (List [App
    "()" (List []),App "Int" (List [App "()" (List []),Int 1,ShowInt (
    Int 1)])])])])])])]),App "Just" (List [App "BDecls" (List [App
    "()" (List []),List [App "PatBind" (List [App "()" (List []),App
    "PVar" (List [App "()" (List []),App "Ident" (List [App "()" (List
    []),String "b"])]),App "UnGuardedRhs" (List [App "()" (List []),
    App "App" (List [App "()" (List []),App "Var" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "fromEnum"])])]),App "Var" (List [App
    "()" (List []),App "UnQual" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),String "a"])])])])]),App "Nothing" (List
    [])])]])])])]])]),App "InsDecl" (List [App "()" (List []),App
    "FunBind" (List [App "()" (List []),List [App "Match" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),String "pred"
    ]),List [App "PVar" (List [App "()" (List []),App "Ident" (List [
    App "()" (List []),String "a"])])],App "UnGuardedRhs" (List [App
    "()" (List []),App "If" (List [App "()" (List []),App "InfixApp" (
    List [App "()" (List []),App "Var" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "b"])])]),App "QVarOp" (List [App "()" (List []),
    App "UnQual" (List [App "()" (List []),App "Symbol" (List [App
    "()" (List []),String "=="])])]),App "Lit" (List [App "()" (List [
    ]),App "Int" (List [App "()" (List []),Int 0,ShowInt (Int 0)])])])
    ,App "App" (List [App "()" (List []),App "Var" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "toEnum"])])]),App "Paren" (List [App
    "()" (List []),App "App" (List [App "()" (List []),App "Var" (List
    [App "()" (List []),App "UnQual" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "length"])])]),App "List"
    (List [App "()" (List []),MapCtor (App "RecConstr" (List [App "()"
    (List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),CtorName])]),List []]))])])])]),App "App" (
    List [App "()" (List []),App "Var" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "toEnum"])])]),App "Paren" (List [App "()" (List [
    ]),App "InfixApp" (List [App "()" (List []),App "Var" (List [App
    "()" (List []),App "UnQual" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),String "b"])])]),App "QVarOp" (List [App
    "()" (List []),App "UnQual" (List [App "()" (List []),App "Symbol"
    (List [App "()" (List []),String "-"])])]),App "Lit" (List [App
    "()" (List []),App "Int" (List [App "()" (List []),Int 1,ShowInt (
    Int 1)])])])])])])]),App "Just" (List [App "BDecls" (List [App
    "()" (List []),List [App "PatBind" (List [App "()" (List []),App
    "PVar" (List [App "()" (List []),App "Ident" (List [App "()" (List
    []),String "b"])]),App "UnGuardedRhs" (List [App "()" (List []),
    App "App" (List [App "()" (List []),App "Var" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "fromEnum"])])]),App "Var" (List [App
    "()" (List []),App "UnQual" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),String "a"])])])])]),App "Nothing" (List
    [])])]])])])]])])]]))]
-- GENERATED STOP
