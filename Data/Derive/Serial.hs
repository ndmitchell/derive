module Data.Derive.Serial where
{-
import {- "smallcheck" -} Test.SmallCheck

example :: Sample

instance Serial a => Serial (Sample a) where
    series = cons0 First \/
             cons2 Second \/
             cons1 Third

    coseries rs d = [ \t -> case t of
                                First -> t0
                                Second x1 x2 -> t1 x1 x2
                                Third x1 -> t2 x1
                    | t0 <- alts0 rs d `const` First{}
                    , t1 <- alts2 rs d `const` Second{}
                    , t2 <- alts1 rs d `const` Third{}
                    ]

-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeSerial :: Derivation
makeSerial = derivationDSL "Serial" dslSerial

dslSerial =
    List [Instance ["Serial"] "Serial" (List [App "InsDecl" (List [App
    "PatBind" (List [App "PVar" (List [App "Ident" (List [String
    "series"])]),App "Nothing" (List []),App "UnGuardedRhs" (List [
    Fold (App "InfixApp" (List [Tail,App "QVarOp" (List [App "UnQual"
    (List [App "Symbol" (List [String "\\/"])])]),Head])) (Reverse (
    MapCtor (App "App" (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [Concat (List [String "cons",ShowInt CtorArity])])])
    ]),App "Con" (List [App "UnQual" (List [App "Ident" (List [
    CtorName])])])]))))]),App "BDecls" (List [List []])])]),App
    "InsDecl" (List [App "FunBind" (List [List [App "Match" (List [App
    "Ident" (List [String "coseries"]),List [App "PVar" (List [App
    "Ident" (List [String "rs"])]),App "PVar" (List [App "Ident" (List
    [String "d"])])],App "Nothing" (List []),App "UnGuardedRhs" (List
    [App "ListComp" (List [App "Lambda" (List [List [App "PVar" (List
    [App "Ident" (List [String "t"])])],App "Case" (List [App "Var" (
    List [App "UnQual" (List [App "Ident" (List [String "t"])])]),
    MapCtor (App "Alt" (List [App "PApp" (List [App "UnQual" (List [
    App "Ident" (List [CtorName])]),MapField (App "PVar" (List [App
    "Ident" (List [Concat (List [String "x",ShowInt FieldIndex])])]))]
    ),App "UnGuardedAlt" (List [Application (Concat (List [List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [Concat (List [
    String "t",ShowInt CtorIndex])])])])],MapField (App "Var" (List [
    App "UnQual" (List [App "Ident" (List [Concat (List [String "x",
    ShowInt FieldIndex])])])]))]))]),App "BDecls" (List [List []])]))]
    )]),Reverse (MapCtor (App "QualStmt" (List [App "Generator" (List
    [App "PVar" (List [App "Ident" (List [Concat (List [String "t",
    ShowInt CtorIndex])])]),App "InfixApp" (List [Application (List [
    App "Var" (List [App "UnQual" (List [App "Ident" (List [Concat (
    List [String "alts",ShowInt CtorArity])])])]),App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "rs"])])]),App "Var" (
    List [App "UnQual" (List [App "Ident" (List [String "d"])])])]),
    App "QVarOp" (List [App "UnQual" (List [App "Ident" (List [String
    "const"])])]),App "RecConstr" (List [App "UnQual" (List [App
    "Ident" (List [CtorName])]),List []])])])])))])]),App "BDecls" (
    List [List []])])]])])])]
-- GENERATED STOP
