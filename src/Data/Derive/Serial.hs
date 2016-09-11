module Data.Derive.Serial where
{-
import "smallcheck" Test.SmallCheck

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
    List [Instance ["Serial"] "Serial" (App "Just" (List [List [App
    "InsDecl" (List [App "()" (List []),App "PatBind" (List [App "()"
    (List []),App "PVar" (List [App "()" (List []),App "Ident" (List [
    App "()" (List []),String "series"])]),App "UnGuardedRhs" (List [
    App "()" (List []),Fold (App "InfixApp" (List [App "()" (List []),
    Tail,App "QVarOp" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Symbol" (List [App "()" (List []),String
    "\\/"])])]),Head])) (Reverse (MapCtor (App "App" (List [App "()" (
    List []),App "Var" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Ident" (List [App "()" (List []),Concat (
    List [String "cons",ShowInt CtorArity])])])]),App "Con" (List [App
    "()" (List []),App "UnQual" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),CtorName])])])]))))]),App "Nothing" (
    List [])])]),App "InsDecl" (List [App "()" (List []),App "FunBind"
    (List [App "()" (List []),List [App "Match" (List [App "()" (List
    []),App "Ident" (List [App "()" (List []),String "coseries"]),List
    [App "PVar" (List [App "()" (List []),App "Ident" (List [App "()"
    (List []),String "rs"])]),App "PVar" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "d"])])],App
    "UnGuardedRhs" (List [App "()" (List []),App "ListComp" (List [App
    "()" (List []),App "Lambda" (List [App "()" (List []),List [App
    "PVar" (List [App "()" (List []),App "Ident" (List [App "()" (List
    []),String "t"])])],App "Case" (List [App "()" (List []),App "Var"
    (List [App "()" (List []),App "UnQual" (List [App "()" (List []),
    App "Ident" (List [App "()" (List []),String "t"])])]),MapCtor (
    App "Alt" (List [App "()" (List []),App "PApp" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),CtorName])]),MapField (App "PVar" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),Concat (List
    [String "x",ShowInt FieldIndex])])]))]),App "UnGuardedRhs" (List [
    App "()" (List []),Application (Concat (List [List [App "Var" (
    List [App "()" (List []),App "UnQual" (List [App "()" (List []),
    App "Ident" (List [App "()" (List []),Concat (List [String "t",
    ShowInt CtorIndex])])])])],MapField (App "Var" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),Concat (List [String "x",ShowInt FieldIndex])]
    )])]))]))]),App "Nothing" (List [])]))])]),MapCtor (App "QualStmt"
    (List [App "()" (List []),App "Generator" (List [App "()" (List []
    ),App "PVar" (List [App "()" (List []),App "Ident" (List [App "()"
    (List []),Concat (List [String "t",ShowInt CtorIndex])])]),App
    "InfixApp" (List [App "()" (List []),Application (List [App "Var"
    (List [App "()" (List []),App "UnQual" (List [App "()" (List []),
    App "Ident" (List [App "()" (List []),Concat (List [String "alts",
    ShowInt CtorArity])])])]),App "Var" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "rs"])])]),App "Var" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "d"])])])]),App "QVarOp" (List [App "()" (List [])
    ,App "UnQual" (List [App "()" (List []),App "Ident" (List [App
    "()" (List []),String "const"])])]),App "RecConstr" (List [App
    "()" (List []),App "UnQual" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),CtorName])]),List []])])])]))])]),App
    "Nothing" (List [])])]])])]]))]
-- GENERATED STOP
