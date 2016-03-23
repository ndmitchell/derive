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

    coarbitrary (First) = variant 0
    coarbitrary (Second x1 x2) = variant 1 . coarbitrary x1 . coarbitrary x2
    coarbitrary (Third x1) = variant 2 . coarbitrary x1

-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeArbitraryOld :: Derivation
makeArbitraryOld = derivationDSL "ArbitraryOld" dslArbitraryOld

dslArbitraryOld =
    List [Instance ["Arbitrary"] "Arbitrary" (List [App "InsDecl" (
    List [App "PatBind" (List [App "PVar" (List [App "Ident" (List [
    String "arbitrary"])]),App "UnGuardedRhs" (List [App "Do" (List [
    List [App "Generator" (List [App "PVar" (List [App "Ident" (List [
    String "x"])]),App "App" (List [App "Var" (List [App "UnQual" (
    List [App "Ident" (List [String "choose"])])]),App "Tuple" (List [
    App "Boxed" (List []),List [App "Lit" (List [App "Int" (List [Int
    0])]),App "InfixApp" (List [App "App" (List [App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "length"])])]),App
    "List" (List [MapCtor (App "RecConstr" (List [App "UnQual" (List [
    App "Ident" (List [CtorName])]),List []]))])]),App "QVarOp" (List
    [App "UnQual" (List [App "Symbol" (List [String "-"])])]),App
    "Lit" (List [App "Int" (List [Int 1])])])]])])]),App "Qualifier" (
    List [App "Case" (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "x"])])]),MapCtor (App "Alt" (List [App
    "PLit" (List [App "Signless" (List []),App "Int" (List [CtorIndex]
    )]),App "UnGuardedRhs" (List [App "Do" (List [Concat (List [
    MapField (App "Generator" (List [App "PVar" (List [App "Ident" (
    List [Concat (List [String "x",ShowInt FieldIndex])])]),App "Var"
    (List [App "UnQual" (List [App "Ident" (List [String "arbitrary"])
    ])])])),List [App "Qualifier" (List [App "App" (List [App "Var" (
    List [App "UnQual" (List [App "Ident" (List [String "return"])])])
    ,App "Paren" (List [Application (Concat (List [List [App "Con" (
    List [App "UnQual" (List [App "Ident" (List [CtorName])])])],
    MapField (App "Var" (List [App "UnQual" (List [App "Ident" (List [
    Concat (List [String "x",ShowInt FieldIndex])])])]))]))])])])]])])
    ]),App "Nothing" (List [])]))])])]])]),App "Nothing" (List [])])])
    ,App "InsDecl" (List [App "FunBind" (List [MapCtor (App "Match" (
    List [App "Ident" (List [String "coarbitrary"]),List [App "PParen"
    (List [App "PApp" (List [App "UnQual" (List [App "Ident" (List [
    CtorName])]),MapField (App "PVar" (List [App "Ident" (List [Concat
    (List [String "x",ShowInt FieldIndex])])]))])])],App "Nothing" (
    List []),App "UnGuardedRhs" (List [Fold (App "InfixApp" (List [
    Head,App "QVarOp" (List [App "UnQual" (List [App "Symbol" (List [
    String "."])])]),Tail])) (Concat (List [List [App "App" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "variant"])])]),App "Lit" (List [App "Int" (List [CtorIndex])])])]
    ,MapField (App "App" (List [App "Var" (List [App "UnQual" (List [
    App "Ident" (List [String "coarbitrary"])])]),App "Var" (List [App
    "UnQual" (List [App "Ident" (List [Concat (List [String "x",
    ShowInt FieldIndex])])])])]))]))]),App "Nothing" (List [])]))])])]
    )]
-- GENERATED STOP
