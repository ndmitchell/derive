{-

{-# PACKAGE derive #-}
{-# MODULE Data.Derive.Class.Default #-}

{-# EXAMPLE #-}

instance Default a => Default (Sample a) where
    def = head [First, Second (const def 1) (const def 2), Third (const def 1)]

{-# TEST [] #-}

instance Default [a] where
    def = []

{-# TEST Bool #-}

instance Default Bool where
    def = False

{-# TEST Either #-}

instance (Default a, Default b) => Default (Either a b) where
    def = Left a

-}

-- GENERATED START

module Data.Derive.Default where

import Language.Haskell.TH.All

dslDefault =
    List [Instance ["Default"] "Default" (List [App "InsDecl" (List
    [App "PatBind" (List [App "PVar" (List [App "Ident" (List [String
    "def"])]),App "Nothing" (List []),App "UnGuardedRhs" (List [App
    "App" (List [App "Var" (List [App "UnQual" (List [App "Ident"
    (List [String "head"])])]),App "List" (List [MapCtor (Application
    (Concat (List [List [App "Con" (List [App "UnQual" (List [App
    "Ident" (List [CtorName])])])],MapField (App "Paren" (List
    [Application (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "const"])])]),App "Var" (List [App "UnQual"
    (List [App "Ident" (List [String "def"])])]),App "Lit" (List [App
    "Int" (List [FieldIndex])])])]))])))])])]),App "BDecls" (List
    [List []])])])])]

makeDefault :: Derivation
makeDefault = undefined
-- GENERATED STOP
