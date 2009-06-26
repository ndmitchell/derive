module Data.Derive.Is where
{-

example :: Sample

isFirst  (First {}) = True; isFirst  _ = False
isSecond (Second{}) = True; isSecond _ = False
isThird  (Third {}) = True; isThird  _ = False

-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeIs :: Derivation
makeIs = derivationDSL "Is" dslIs

dslIs =
    MapCtor (App "FunBind" (List [List [App "Match" (List [App "Ident"
    (List [Concat (List [String "is",CtorName])]),List [App "PParen" (
    List [App "PRec" (List [App "UnQual" (List [App "Ident" (List [
    CtorName])]),List []])])],App "Nothing" (List []),App
    "UnGuardedRhs" (List [App "Con" (List [App "UnQual" (List [App
    "Ident" (List [String "True"])])])]),App "BDecls" (List [List []])
    ]),App "Match" (List [App "Ident" (List [Concat (List [String "is"
    ,CtorName])]),List [App "PWildCard" (List [])],App "Nothing" (List
    []),App "UnGuardedRhs" (List [App "Con" (List [App "UnQual" (List
    [App "Ident" (List [String "False"])])])]),App "BDecls" (List [
    List []])])]]))
-- GENERATED STOP
