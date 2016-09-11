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
    List [Instance ["Monoid"] "Monoid" (App "Just" (List [List [App
    "InsDecl" (List [App "()" (List []),App "PatBind" (List [App "()"
    (List []),App "PVar" (List [App "()" (List []),App "Ident" (List [
    App "()" (List []),String "mempty"])]),App "UnGuardedRhs" (List [
    App "()" (List []),App "App" (List [App "()" (List []),App "Var" (
    List [App "()" (List []),App "UnQual" (List [App "()" (List []),
    App "Ident" (List [App "()" (List []),String "head"])])]),App
    "List" (List [App "()" (List []),MapCtor (Application (Concat (
    List [List [App "Con" (List [App "()" (List []),App "UnQual" (List
    [App "()" (List []),App "Ident" (List [App "()" (List []),CtorName
    ])])])],MapField (App "Paren" (List [App "()" (List []),
    Application (List [App "Var" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "const"])])]),App "Var" (List [App "()" (List []),
    App "UnQual" (List [App "()" (List []),App "Ident" (List [App "()"
    (List []),String "mempty"])])]),App "Lit" (List [App "()" (List []
    ),App "Int" (List [App "()" (List []),FieldIndex,ShowInt
    FieldIndex])])])]))])))])])]),App "Nothing" (List [])])]),App
    "InsDecl" (List [App "()" (List []),App "FunBind" (List [App "()"
    (List []),Concat (List [MapCtor (App "Match" (List [App "()" (List
    []),App "Ident" (List [App "()" (List []),String "mappend"]),List
    [App "PParen" (List [App "()" (List []),App "PApp" (List [App "()"
    (List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),CtorName])]),MapField (App "PVar" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),Concat (List
    [String "x",ShowInt FieldIndex])])]))])]),App "PParen" (List [App
    "()" (List []),App "PApp" (List [App "()" (List []),App "UnQual" (
    List [App "()" (List []),App "Ident" (List [App "()" (List []),
    CtorName])]),MapField (App "PVar" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),Concat (List [String "y",ShowInt
    FieldIndex])])]))])])],App "UnGuardedRhs" (List [App "()" (List []
    ),Application (Concat (List [List [App "Con" (List [App "()" (List
    []),App "UnQual" (List [App "()" (List []),App "Ident" (List [App
    "()" (List []),CtorName])])])],MapField (App "Paren" (List [App
    "()" (List []),Application (List [App "Var" (List [App "()" (List
    []),App "UnQual" (List [App "()" (List []),App "Ident" (List [App
    "()" (List []),String "mappend"])])]),App "Var" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),Concat (List [String "x",ShowInt FieldIndex])]
    )])]),App "Var" (List [App "()" (List []),App "UnQual" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),Concat (List
    [String "y",ShowInt FieldIndex])])])])])]))]))]),App "Nothing" (
    List [])])),List [App "Match" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "mappend"]),List [App
    "PWildCard" (List [App "()" (List [])]),App "PWildCard" (List [App
    "()" (List [])])],App "GuardedRhss" (List [App "()" (List []),List
    [App "GuardedRhs" (List [App "()" (List []),List [App "Qualifier"
    (List [App "()" (List []),App "InfixApp" (List [App "()" (List [])
    ,App "App" (List [App "()" (List []),App "Var" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "length"])])]),App "List" (List [App
    "()" (List []),MapCtor (App "RecConstr" (List [App "()" (List []),
    App "UnQual" (List [App "()" (List []),App "Ident" (List [App "()"
    (List []),CtorName])]),List []]))])]),App "QVarOp" (List [App "()"
    (List []),App "UnQual" (List [App "()" (List []),App "Symbol" (
    List [App "()" (List []),String ">"])])]),App "Lit" (List [App
    "()" (List []),App "Int" (List [App "()" (List []),Int 1,ShowInt (
    Int 1)])])])])],App "App" (List [App "()" (List []),App "Var" (
    List [App "()" (List []),App "UnQual" (List [App "()" (List []),
    App "Ident" (List [App "()" (List []),String "error"])])]),App
    "Lit" (List [App "()" (List []),App "String" (List [App "()" (List
    []),Concat (List [String
    "Monoid.mappend: Different constructors for ",DataName]),Concat (
    List [String "Monoid.mappend: Different constructors for ",
    DataName])])])])])]]),App "Nothing" (List [])])]])])])]]))]
-- GENERATED STOP
