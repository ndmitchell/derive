module Data.Derive.Arbitrary(makeArbitrary) where
{-
import "QuickCheck" Test.QuickCheck

example :: Custom

instance Arbitrary (Sample a) where
    arbitrary = do
        x <- choose (0::Int,length [First{},Second{},Third{}] - 1)
        case x of
            0 -> do return (First)
            1 -> do x1 <- arbitrary
                    x2 <- arbitrary
                    return (Second x1 x2)
            2 -> do x1 <- arbitrary
                    return (Third x1)
            _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

test :: State
instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (State s a) where
    arbitrary = do x1 <- arbitrary
                   return (StateT x1)
-}

import Data.Derive.DSL.HSE
import Data.List
import Data.Generics.Uniplate.DataOnly

-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeArbitrary :: Derivation
makeArbitrary = derivationCustomDSL "Arbitrary" custom $
    List [Instance [] "Arbitrary" (App "Just" (List [List [App
    "InsDecl" (List [App "()" (List []),App "PatBind" (List [App "()"
    (List []),App "PVar" (List [App "()" (List []),App "Ident" (List [
    App "()" (List []),String "arbitrary"])]),App "UnGuardedRhs" (List
    [App "()" (List []),App "Do" (List [App "()" (List []),List [App
    "Generator" (List [App "()" (List []),App "PVar" (List [App "()" (
    List []),App "Ident" (List [App "()" (List []),String "x"])]),App
    "App" (List [App "()" (List []),App "Var" (List [App "()" (List []
    ),App "UnQual" (List [App "()" (List []),App "Ident" (List [App
    "()" (List []),String "choose"])])]),App "Tuple" (List [App "()" (
    List []),App "Boxed" (List []),List [App "ExpTypeSig" (List [App
    "()" (List []),App "Lit" (List [App "()" (List []),App "Int" (List
    [App "()" (List []),Int 0,ShowInt (Int 0)])]),App "TyCon" (List [
    App "()" (List []),App "UnQual" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "Int"])])])]),App
    "InfixApp" (List [App "()" (List []),App "App" (List [App "()" (
    List []),App "Var" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Ident" (List [App "()" (List []),String
    "length"])])]),App "List" (List [App "()" (List []),MapCtor (App
    "RecConstr" (List [App "()" (List []),App "UnQual" (List [App "()"
    (List []),App "Ident" (List [App "()" (List []),CtorName])]),List
    []]))])]),App "QVarOp" (List [App "()" (List []),App "UnQual" (
    List [App "()" (List []),App "Symbol" (List [App "()" (List []),
    String "-"])])]),App "Lit" (List [App "()" (List []),App "Int" (
    List [App "()" (List []),Int 1,ShowInt (Int 1)])])])]])])]),App
    "Qualifier" (List [App "()" (List []),App "Case" (List [App "()" (
    List []),App "Var" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Ident" (List [App "()" (List []),String
    "x"])])]),Concat (List [MapCtor (App "Alt" (List [App "()" (List [
    ]),App "PLit" (List [App "()" (List []),App "Signless" (List [App
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
    )]))])])])]])])]),App "Nothing" (List [])])),List [App "Alt" (List
    [App "()" (List []),App "PWildCard" (List [App "()" (List [])]),
    App "UnGuardedRhs" (List [App "()" (List []),App "App" (List [App
    "()" (List []),App "Var" (List [App "()" (List []),App "UnQual" (
    List [App "()" (List []),App "Ident" (List [App "()" (List []),
    String "error"])])]),App "Lit" (List [App "()" (List []),App
    "String" (List [App "()" (List []),String
    "FATAL ERROR: Arbitrary instance, logic bug",String
    "FATAL ERROR: Arbitrary instance, logic bug"])])])]),App "Nothing"
    (List [])])]])])])]])]),App "Nothing" (List [])])])]]))]
-- GENERATED STOP

custom = customContext context

-- Fix the context
-- C a b => Arbitrary a, Arbitrary b
-- a -> b => CoArbitrary a, Arbitrary b
context :: FullDataDecl -> Context () -> Context ()
context (_,d) _ = CxTuple () $ nub $ concatMap (f True . snd) $ concatMap ctorDeclFields $ dataDeclCtors d
    where
        f b (TyVar _ x) = [ClassA () (qname $ b ? "Arbitrary" $ "CoArbitrary") [TyVar () x]]
        f b (TyFun _ x y) = f (not b) x ++ f b y
        f b x = concatMap (f b) (children x)

