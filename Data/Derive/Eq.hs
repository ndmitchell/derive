module Data.Derive.Eq where
{-
import Prelude

{-# EXAMPLE #-}

instance (Eq a) => Eq (Sample a) where
    First == First = True
    Second x1 x2 == Second y1 y2 = (x1 == y1) && (x2 == y2) && True
    Third x1 == Third y1 = (x1 == y1) && True
    _ == _ = False

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
    []),App "UnGuardedRhs" (List [Fold (App "InfixApp" (List [Tail,App
    "QVarOp" (List [App "UnQual" (List [App "Symbol" (List [String
    "&&"])])]),Head])) (Concat (List [List [App "Con" (List [App
    "UnQual" (List [App "Ident" (List [String "True"])])])],Reverse (
    MapField (App "Paren" (List [App "InfixApp" (List [App "Var" (List
    [App "UnQual" (List [App "Ident" (List [Concat (List [String "x",
    ShowInt FieldIndex])])])]),App "QVarOp" (List [App "UnQual" (List
    [App "Symbol" (List [String "=="])])]),App "Var" (List [App
    "UnQual" (List [App "Ident" (List [Concat (List [String "y",
    ShowInt FieldIndex])])])])])])))]))]),App "BDecls" (List [List []]
    )])),List [App "Match" (List [App "Symbol" (List [String "=="]),
    List [App "PWildCard" (List []),App "PWildCard" (List [])],App
    "Nothing" (List []),App "UnGuardedRhs" (List [App "Con" (List [App
    "UnQual" (List [App "Ident" (List [String "False"])])])]),App
    "BDecls" (List [List []])])]])])])])]
-- GENERATED STOP
