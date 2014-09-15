{-|
    Derives an instance of @Monoid@. It uses the product
    construction of monoids. @mappend@ on two different constructors
    is undefined.
-}
module Data.Derive.Monoid(makeMonoid) where
{-
import Data.Monoid hiding (First)

example :: Sample
instance Monoid a => Monoid (Sample a) where
    mempty = head [First, Second (const mempty 1) (const mempty 2), Third (const mempty 1)]
    mappend (First) (First) = First
    mappend (Second x1 x2) (Second y1 y2) = Second (mappend x1 y1) (mappend x2 y2)
    mappend (Third x1) (Third y1) = Third (mappend x1 y1)
    mappend _ _ | length [First{},Second{},Third{}] > 1 = error "Monoid.mappend: Different constructors for Sample"

-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeMonoid :: Derivation
makeMonoid = derivationDSL "Monoid" dslMonoid

dslMonoid =
    List [Instance ["Monoid"] "Monoid" (List [App "InsDecl" (List [App
    "PatBind" (List [App "PVar" (List [App "Ident" (List [String
    "mempty"])]),App "UnGuardedRhs" (List [App "App" (List [App "Var"
    (List [App "UnQual" (List [App "Ident" (List [String "head"])])]),
    App "List" (List [MapCtor (Application (Concat (List [List [App
    "Con" (List [App "UnQual" (List [App "Ident" (List [CtorName])])])
    ],MapField (App "Paren" (List [Application (List [App "Var" (List
    [App "UnQual" (List [App "Ident" (List [String "const"])])]),App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "mempty"])])]),App "Lit" (List [App "Int" (List [FieldIndex])])])]
    ))])))])])]),App "BDecls" (List [List []])])]),App "InsDecl" (List
    [App "FunBind" (List [Concat (List [MapCtor (App "Match" (List [
    App "Ident" (List [String "mappend"]),List [App "PParen" (List [
    App "PApp" (List [App "UnQual" (List [App "Ident" (List [CtorName]
    )]),MapField (App "PVar" (List [App "Ident" (List [Concat (List [
    String "x",ShowInt FieldIndex])])]))])]),App "PParen" (List [App
    "PApp" (List [App "UnQual" (List [App "Ident" (List [CtorName])]),
    MapField (App "PVar" (List [App "Ident" (List [Concat (List [
    String "y",ShowInt FieldIndex])])]))])])],App "Nothing" (List []),
    App "UnGuardedRhs" (List [Application (Concat (List [List [App
    "Con" (List [App "UnQual" (List [App "Ident" (List [CtorName])])])
    ],MapField (App "Paren" (List [Application (List [App "Var" (List
    [App "UnQual" (List [App "Ident" (List [String "mappend"])])]),App
    "Var" (List [App "UnQual" (List [App "Ident" (List [Concat (List [
    String "x",ShowInt FieldIndex])])])]),App "Var" (List [App
    "UnQual" (List [App "Ident" (List [Concat (List [String "y",
    ShowInt FieldIndex])])])])])]))]))]),App "BDecls" (List [List []])
    ])),List [App "Match" (List [App "Ident" (List [String "mappend"])
    ,List [App "PWildCard" (List []),App "PWildCard" (List [])],App
    "Nothing" (List []),App "GuardedRhss" (List [List [App
    "GuardedRhs" (List [List [App "Qualifier" (List [App "InfixApp" (
    List [App "App" (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "length"])])]),App "List" (List [MapCtor (
    App "RecConstr" (List [App "UnQual" (List [App "Ident" (List [
    CtorName])]),List []]))])]),App "QVarOp" (List [App "UnQual" (List
    [App "Symbol" (List [String ">"])])]),App "Lit" (List [App "Int" (
    List [Int 1])])])])],App "App" (List [App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "error"])])]),App "Lit"
    (List [App "String" (List [Concat (List [String
    "Monoid.mappend: Different constructors for ",DataName])])])])])]]
    ),App "BDecls" (List [List []])])]])])])])]
-- GENERATED STOP
