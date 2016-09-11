module Data.Derive.ArbitraryOld where
{-
import "QuickCheck-1.2.0.0" Test.QuickCheck(Arbitrary(..), choose,variant)

example :: Sample

instance Arbitrary a => Arbitrary (Sample a) where
    arbitrary = do
        x <- choose (0,length [First{},Second{},Third{}]-1)
        case x of
            0 -> do return (First)
            1 -> do x1 <- arbitrary
                    x2 <- arbitrary
                    return (Second x1 x2)
            2 -> do x1 <- arbitrary
                    return (Third x1)

    coarbitrary (First) = ()
    coarbitrary (Second x1 x2) = ()
    coarbitrary (Third x1) = ()

-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeArbitraryOld :: Derivation
makeArbitraryOld = derivationDSL "ArbitraryOld" dslArbitraryOld

dslArbitraryOld =
    List [Instance ["Arbitrary"] "Arbitrary" (App "Just" (List [List [
    App "InsDecl" (List [App "()" (List []),App "PatBind" (List [App
    "()" (List []),App "PVar" (List [App "()" (List []),App "Ident" (
    List [App "()" (List []),String "arbitrary"])]),App "UnGuardedRhs"
    (List [App "()" (List []),App "Do" (List [App "()" (List []),List
    [App "Generator" (List [App "()" (List []),App "PVar" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),String "x"])]
    ),App "App" (List [App "()" (List []),App "Var" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "choose"])])]),App "Tuple" (List [App
    "()" (List []),App "Boxed" (List []),List [App "Lit" (List [App
    "()" (List []),App "Int" (List [App "()" (List []),Int 0,ShowInt (
    Int 0)])]),App "InfixApp" (List [App "()" (List []),App "App" (
    List [App "()" (List []),App "Var" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "length"])])]),App "List" (List [App "()" (List []
    ),MapCtor (App "RecConstr" (List [App "()" (List []),App "UnQual"
    (List [App "()" (List []),App "Ident" (List [App "()" (List []),
    CtorName])]),List []]))])]),App "QVarOp" (List [App "()" (List [])
    ,App "UnQual" (List [App "()" (List []),App "Symbol" (List [App
    "()" (List []),String "-"])])]),App "Lit" (List [App "()" (List []
    ),App "Int" (List [App "()" (List []),Int 1,ShowInt (Int 1)])])])]
    ])])]),App "Qualifier" (List [App "()" (List []),App "Case" (List
    [App "()" (List []),App "Var" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "x"])])]),MapCtor (App "Alt" (List [App "()" (List
    []),App "PLit" (List [App "()" (List []),App "Signless" (List [App
    "()" (List [])]),App "Int" (List [App "()" (List []),CtorIndex,
    ShowInt CtorIndex])]),App "UnGuardedRhs" (List [App "()" (List [])
    ,App "Do" (List [App "()" (List []),Concat (List [MapField (App
    "Generator" (List [App "()" (List []),App "PVar" (List [App "()" (
    List []),App "Ident" (List [App "()" (List []),Concat (List [
    String "x",ShowInt FieldIndex])])]),App "Var" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "arbitrary"])])])])),List [App
    "Qualifier" (List [App "()" (List []),App "App" (List [App "()" (
    List []),App "Var" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Ident" (List [App "()" (List []),String
    "return"])])]),App "Paren" (List [App "()" (List []),Application (
    Concat (List [List [App "Con" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),CtorName])])])],MapField (App "Var" (List [App "()" (List
    []),App "UnQual" (List [App "()" (List []),App "Ident" (List [App
    "()" (List []),Concat (List [String "x",ShowInt FieldIndex])])])])
    )]))])])])]])])]),App "Nothing" (List [])]))])])]])]),App
    "Nothing" (List [])])]),App "InsDecl" (List [App "()" (List []),
    App "FunBind" (List [App "()" (List []),MapCtor (App "Match" (List
    [App "()" (List []),App "Ident" (List [App "()" (List []),String
    "coarbitrary"]),List [App "PParen" (List [App "()" (List []),App
    "PApp" (List [App "()" (List []),App "UnQual" (List [App "()" (
    List []),App "Ident" (List [App "()" (List []),CtorName])]),
    MapField (App "PVar" (List [App "()" (List []),App "Ident" (List [
    App "()" (List []),Concat (List [String "x",ShowInt FieldIndex])])
    ]))])])],App "UnGuardedRhs" (List [App "()" (List []),App "Con" (
    List [App "()" (List []),App "Special" (List [App "()" (List []),
    App "UnitCon" (List [App "()" (List [])])])])]),App "Nothing" (
    List [])]))])])]]))]
-- GENERATED STOP
