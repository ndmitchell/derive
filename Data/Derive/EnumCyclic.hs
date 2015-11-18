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
    List [Instance [] "Enum" (List [App "InsDecl" (List [App "FunBind"
    (List [Concat (List [MapCtor (App "Match" (List [App "Ident" (List
    [String "toEnum"]),List [App "PLit" (List [App "Signless" (List []
    ),App "Int" (List [CtorIndex])])],App "Nothing" (List []),App
    "UnGuardedRhs" (List [App "RecConstr" (List [App "UnQual" (List [
    App "Ident" (List [CtorName])]),List []])]),App "Nothing" (List []
    )])),List [App "Match" (List [App "Ident" (List [String "toEnum"])
    ,List [App "PVar" (List [App "Ident" (List [String "n"])])],App
    "Nothing" (List []),App "UnGuardedRhs" (List [App "InfixApp" (List
    [App "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "error"])])]),App "QVarOp" (List [App "UnQual" (List [App "Symbol"
    (List [String "$"])])]),Fold (App "InfixApp" (List [Head,App
    "QVarOp" (List [App "UnQual" (List [App "Symbol" (List [String
    "++"])])]),Tail])) (List [App "Lit" (List [App "String" (List [
    String "toEnum "])]),App "App" (List [App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "show"])])]),App "Var" (
    List [App "UnQual" (List [App "Ident" (List [String "n"])])])]),
    App "Lit" (List [App "String" (List [Concat (List [String
    ", not defined for ",DataName])])])])])]),App "Nothing" (List [])]
    )]])])]),App "InsDecl" (List [App "FunBind" (List [MapCtor (App
    "Match" (List [App "Ident" (List [String "fromEnum"]),List [App
    "PParen" (List [App "PRec" (List [App "UnQual" (List [App "Ident"
    (List [CtorName])]),List []])])],App "Nothing" (List []),App
    "UnGuardedRhs" (List [App "Lit" (List [App "Int" (List [CtorIndex]
    )])]),App "Nothing" (List [])]))])]),App "InsDecl" (List [App
    "FunBind" (List [List [App "Match" (List [App "Ident" (List [
    String "succ"]),List [App "PVar" (List [App "Ident" (List [String
    "a"])])],App "Nothing" (List []),App "UnGuardedRhs" (List [App
    "If" (List [App "InfixApp" (List [App "Var" (List [App "UnQual" (
    List [App "Ident" (List [String "b"])])]),App "QVarOp" (List [App
    "UnQual" (List [App "Symbol" (List [String "=="])])]),App "App" (
    List [App "Var" (List [App "UnQual" (List [App "Ident" (List [
    String "length"])])]),App "List" (List [MapCtor (App "RecConstr" (
    List [App "UnQual" (List [App "Ident" (List [CtorName])]),List []]
    ))])])]),App "App" (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "toEnum"])])]),App "Lit" (List [App "Int" (
    List [Int 0])])]),App "App" (List [App "Var" (List [App "UnQual" (
    List [App "Ident" (List [String "toEnum"])])]),App "Paren" (List [
    App "InfixApp" (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "b"])])]),App "QVarOp" (List [App "UnQual" (
    List [App "Symbol" (List [String "+"])])]),App "Lit" (List [App
    "Int" (List [Int 1])])])])])])]),App "Just" (List [App "BDecls" (
    List [List [App "PatBind" (List [App "PVar" (List [App "Ident" (
    List [String "b"])]),App "UnGuardedRhs" (List [App "App" (List [
    App "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "fromEnum"])])]),App "Var" (List [App "UnQual" (List [App "Ident"
    (List [String "a"])])])])]),App "Nothing" (List [])])]])])])]])]),
    App "InsDecl" (List [App "FunBind" (List [List [App "Match" (List
    [App "Ident" (List [String "pred"]),List [App "PVar" (List [App
    "Ident" (List [String "a"])])],App "Nothing" (List []),App
    "UnGuardedRhs" (List [App "If" (List [App "InfixApp" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String "b"])])
    ]),App "QVarOp" (List [App "UnQual" (List [App "Symbol" (List [
    String "=="])])]),App "Lit" (List [App "Int" (List [Int 0])])]),
    App "App" (List [App "Var" (List [App "UnQual" (List [App "Ident"
    (List [String "toEnum"])])]),App "Paren" (List [App "App" (List [
    App "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "length"])])]),App "List" (List [MapCtor (App "RecConstr" (List [
    App "UnQual" (List [App "Ident" (List [CtorName])]),List []]))])])
    ])]),App "App" (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "toEnum"])])]),App "Paren" (List [App
    "InfixApp" (List [App "Var" (List [App "UnQual" (List [App "Ident"
    (List [String "b"])])]),App "QVarOp" (List [App "UnQual" (List [
    App "Symbol" (List [String "-"])])]),App "Lit" (List [App "Int" (
    List [Int 1])])])])])])]),App "Just" (List [App "BDecls" (List [
    List [App "PatBind" (List [App "PVar" (List [App "Ident" (List [
    String "b"])]),App "UnGuardedRhs" (List [App "App" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "fromEnum"])])]),App "Var" (List [App "UnQual" (List [App "Ident"
    (List [String "a"])])])])]),App "Nothing" (List [])])]])])])]])])]
    )]
-- GENERATED STOP
