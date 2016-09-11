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
    List [Instance ["Bounded"] "Bounded" (App "Just" (List [List [App
    "InsDecl" (List [App "()" (List []),App "PatBind" (List [App "()"
    (List []),App "PVar" (List [App "()" (List []),App "Ident" (List [
    App "()" (List []),String "minBound"])]),App "UnGuardedRhs" (List
    [App "()" (List []),App "App" (List [App "()" (List []),App "Var"
    (List [App "()" (List []),App "UnQual" (List [App "()" (List []),
    App "Ident" (List [App "()" (List []),String "head"])])]),App
    "List" (List [App "()" (List []),MapCtor (Application (Concat (
    List [List [App "Con" (List [App "()" (List []),App "UnQual" (List
    [App "()" (List []),App "Ident" (List [App "()" (List []),CtorName
    ])])])],MapField (App "Paren" (List [App "()" (List []),
    Application (List [App "Var" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "const"])])]),App "Var" (List [App "()" (List []),
    App "UnQual" (List [App "()" (List []),App "Ident" (List [App "()"
    (List []),String "minBound"])])]),App "Lit" (List [App "()" (List
    []),App "Int" (List [App "()" (List []),FieldIndex,ShowInt
    FieldIndex])])])]))])))])])]),App "Nothing" (List [])])]),App
    "InsDecl" (List [App "()" (List []),App "PatBind" (List [App "()"
    (List []),App "PVar" (List [App "()" (List []),App "Ident" (List [
    App "()" (List []),String "maxBound"])]),App "UnGuardedRhs" (List
    [App "()" (List []),App "App" (List [App "()" (List []),App "Var"
    (List [App "()" (List []),App "UnQual" (List [App "()" (List []),
    App "Ident" (List [App "()" (List []),String "head"])])]),App
    "List" (List [App "()" (List []),Reverse (MapCtor (Application (
    Concat (List [List [App "Con" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),CtorName])])])],MapField (App "Paren" (List [App "()" (
    List []),Application (List [App "Var" (List [App "()" (List []),
    App "UnQual" (List [App "()" (List []),App "Ident" (List [App "()"
    (List []),String "const"])])]),App "Var" (List [App "()" (List [])
    ,App "UnQual" (List [App "()" (List []),App "Ident" (List [App
    "()" (List []),String "maxBound"])])]),App "Lit" (List [App "()" (
    List []),App "Int" (List [App "()" (List []),FieldIndex,ShowInt
    FieldIndex])])])]))]))))])])]),App "Nothing" (List [])])])]]))]
-- GENERATED STOP
