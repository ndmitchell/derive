module Data.Derive.Arbitrary where
{-
import "QuickCheck" Test.QuickCheck

example :: Sample

instance Arbitrary a => Arbitrary (Sample a) where
    arbitrary = do
        x <- choose (0::Int,length [First{},Second{},Third{}] - 1)
        case x of
            0 -> do return (First)
            1 -> do x1 <- arbitrary
                    x2 <- arbitrary
                    return (Second x1 x2)
            2 -> do x1 <- arbitrary
                    return (Third x1)

-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeArbitrary :: Derivation
makeArbitrary = derivationDSL "Arbitrary" dslArbitrary

dslArbitrary =
    List [Instance ["Arbitrary"] "Arbitrary" (List [App "InsDecl" (
    List [App "PatBind" (List [App "PVar" (List [App "Ident" (List [
    String "arbitrary"])]),App "Nothing" (List []),App "UnGuardedRhs"
    (List [App "Do" (List [List [App "Generator" (List [App "PVar" (
    List [App "Ident" (List [String "x"])]),App "App" (List [App "Var"
    (List [App "UnQual" (List [App "Ident" (List [String "choose"])])]
    ),App "Tuple" (List [List [App "ExpTypeSig" (List [App "Lit" (List
    [App "Int" (List [Int 0])]),App "TyCon" (List [App "UnQual" (List
    [App "Ident" (List [String "Int"])])])]),App "InfixApp" (List [App
    "App" (List [App "Var" (List [App "UnQual" (List [App "Ident" (
    List [String "length"])])]),App "List" (List [MapCtor (App
    "RecConstr" (List [App "UnQual" (List [App "Ident" (List [CtorName
    ])]),List []]))])]),App "QVarOp" (List [App "UnQual" (List [App
    "Symbol" (List [String "-"])])]),App "Lit" (List [App "Int" (List
    [Int 1])])])]])])]),App "Qualifier" (List [App "Case" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String "x"])])
    ]),MapCtor (App "Alt" (List [App "PLit" (List [App "Int" (List [
    CtorIndex])]),App "UnGuardedAlt" (List [App "Do" (List [Concat (
    List [MapField (App "Generator" (List [App "PVar" (List [App
    "Ident" (List [Concat (List [String "x",ShowInt FieldIndex])])]),
    App "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "arbitrary"])])])])),List [App "Qualifier" (List [App "App" (List
    [App "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "return"])])]),App "Paren" (List [Application (Concat (List [List
    [App "Con" (List [App "UnQual" (List [App "Ident" (List [CtorName]
    )])])],MapField (App "Var" (List [App "UnQual" (List [App "Ident"
    (List [Concat (List [String "x",ShowInt FieldIndex])])])]))]))])])
    ])]])])]),App "BDecls" (List [List []])]))])])]])]),App "BDecls" (
    List [List []])])])])]
-- GENERATED STOP
