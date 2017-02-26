module Data.Derive.Default where
{-
import "derive" Data.Derive.Class.Default

example :: Sample

instance Default a => Default (Sample a) where
    def = head [First, Second (const def 1) (const def 2), Third (const def 1)]
-}

-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeDefault :: Derivation
makeDefault = derivationDSL "Default" dslDefault

dslDefault =
    List [Instance ["Default"] "Default" (App "Just" (List [List [App
    "InsDecl" (List [App "()" (List []),App "PatBind" (List [App "()"
    (List []),App "PVar" (List [App "()" (List []),App "Ident" (List [
    App "()" (List []),String "def"])]),App "UnGuardedRhs" (List [App
    "()" (List []),App "App" (List [App "()" (List []),App "Var" (List
    [App "()" (List []),App "UnQual" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "head"])])]),App "List" (
    List [App "()" (List []),MapCtor (Application (Concat (List [List
    [App "Con" (List [App "()" (List []),App "UnQual" (List [App "()"
    (List []),App "Ident" (List [App "()" (List []),CtorName])])])],
    MapField (App "Paren" (List [App "()" (List []),Application (List
    [App "Var" (List [App "()" (List []),App "UnQual" (List [App "()"
    (List []),App "Ident" (List [App "()" (List []),String "const"])])
    ]),App "Var" (List [App "()" (List []),App "UnQual" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),String "def"]
    )])]),App "Lit" (List [App "()" (List []),App "Int" (List [App
    "()" (List []),FieldIndex,ShowInt FieldIndex])])])]))])))])])]),
    App "Nothing" (List [])])])]]))]
-- GENERATED STOP
