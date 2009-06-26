module Data.Derive.Bounded where
{-
import Prelude

example :: Sample

instance Bounded a => Bounded (Sample a) where
    minBound = head [First, Second (const minBound 1) (const minBound 2), Third (const minBound 1)]
    maxBound = head [Third (const maxBound 1), Second (const maxBound 1) (const maxBound 2), First]

-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeBounded :: Derivation
makeBounded = derivationDSL "Bounded" dslBounded

dslBounded =
    List [Instance ["Bounded"] "Bounded" (List [App "InsDecl" (List [
    App "PatBind" (List [App "PVar" (List [App "Ident" (List [String
    "minBound"])]),App "Nothing" (List []),App "UnGuardedRhs" (List [
    App "App" (List [App "Var" (List [App "UnQual" (List [App "Ident"
    (List [String "head"])])]),App "List" (List [MapCtor (Application
    (Concat (List [List [App "Con" (List [App "UnQual" (List [App
    "Ident" (List [CtorName])])])],MapField (App "Paren" (List [
    Application (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "const"])])]),App "Var" (List [App "UnQual"
    (List [App "Ident" (List [String "minBound"])])]),App "Lit" (List
    [App "Int" (List [FieldIndex])])])]))])))])])]),App "BDecls" (List
    [List []])])]),App "InsDecl" (List [App "PatBind" (List [App
    "PVar" (List [App "Ident" (List [String "maxBound"])]),App
    "Nothing" (List []),App "UnGuardedRhs" (List [App "App" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String "head"]
    )])]),App "List" (List [Reverse (MapCtor (Application (Concat (
    List [List [App "Con" (List [App "UnQual" (List [App "Ident" (List
    [CtorName])])])],MapField (App "Paren" (List [Application (List [
    App "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "const"])])]),App "Var" (List [App "UnQual" (List [App "Ident" (
    List [String "maxBound"])])]),App "Lit" (List [App "Int" (List [
    FieldIndex])])])]))]))))])])]),App "BDecls" (List [List []])])])])
    ]
-- GENERATED STOP
