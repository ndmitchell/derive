{-|
    Derives an instance of @Monoid@. It uses the product
    construction of monoids. @mappend@ on two different constructors
    is undefined.
-}
module Data.Derive.Monoid(makeMonoid) where

import Language.Haskell.TH.All

{-
{-# EXAMPLE #-}

instance Monoid a => Monoid (Sample a) where
    mempty = head [First, Second (const mempty 1) (const mempty 2), Third (const mempty 1)]
    mappend (First) (First) = First
    mappend (Second x1 x2) (Second y1 y2) = Second (mappend x1 y1) (mappend x2 y2)
    mappend (Third x1) (Third y1) = Third (mappend x1 y1)

-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeMonoid :: Derivation
makeMonoid = derivationDSL "Monoid" dslMonoid

dslMonoid =
    List [Instance ["Monoid"] "Monoid" (List [App "InsDecl" (List [App
    "PatBind" (List [App "PVar" (List [App "Ident" (List [String
    "mempty"])]),App "Nothing" (List []),App "UnGuardedRhs" (List [App
    "App" (List [App "Var" (List [App "UnQual" (List [App "Ident" (
    List [String "head"])])]),App "List" (List [MapCtor (Application (
    Concat (List [List [App "Con" (List [App "UnQual" (List [App
    "Ident" (List [CtorName])])])],MapField (App "Paren" (List [
    Application (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "const"])])]),App "Var" (List [App "UnQual"
    (List [App "Ident" (List [String "mempty"])])]),App "Lit" (List [
    App "Int" (List [FieldIndex])])])]))])))])])]),App "BDecls" (List
    [List []])])]),App "InsDecl" (List [App "FunBind" (List [MapCtor (
    App "Match" (List [App "Ident" (List [String "mappend"]),List [App
    "PParen" (List [App "PApp" (List [App "UnQual" (List [App "Ident"
    (List [CtorName])]),MapField (App "PVar" (List [App "Ident" (List
    [Concat (List [String "x",ShowInt FieldIndex])])]))])]),App
    "PParen" (List [App "PApp" (List [App "UnQual" (List [App "Ident"
    (List [CtorName])]),MapField (App "PVar" (List [App "Ident" (List
    [Concat (List [String "y",ShowInt FieldIndex])])]))])])],App
    "Nothing" (List []),App "UnGuardedRhs" (List [Application (Concat
    (List [List [App "Con" (List [App "UnQual" (List [App "Ident" (
    List [CtorName])])])],MapField (App "Paren" (List [Application (
    List [App "Var" (List [App "UnQual" (List [App "Ident" (List [
    String "mappend"])])]),App "Var" (List [App "UnQual" (List [App
    "Ident" (List [Concat (List [String "x",ShowInt FieldIndex])])])])
    ,App "Var" (List [App "UnQual" (List [App "Ident" (List [Concat (
    List [String "y",ShowInt FieldIndex])])])])])]))]))]),App "BDecls"
    (List [List []])]))])])])]
-- GENERATED STOP
