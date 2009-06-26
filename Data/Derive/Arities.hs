module Data.Derive.Arities where
{-
import "derive" Data.Derive.Class.Arities

example :: Sample

instance Arities (Sample a) where
    arities _ = [const 0 First{}, const 2 Second{}, const 1 Third{}]

test :: []

instance Arities [a] where
    arities _ = [0,2]

test :: Bool

instance Arities Bool where
    arities _ = [0,0]

test :: Either

instance Arities (Either a b) where
    arities _ = [1,1]
-}

-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeArities :: Derivation
makeArities = derivationDSL "Arities" dslArities

dslArities =
    List [Instance [] "Arities" (List [App "InsDecl" (List [App
    "FunBind" (List [List [App "Match" (List [App "Ident" (List [
    String "arities"]),List [App "PWildCard" (List [])],App "Nothing"
    (List []),App "UnGuardedRhs" (List [App "List" (List [MapCtor (
    Application (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "const"])])]),App "Lit" (List [App "Int" (
    List [CtorArity])]),App "RecConstr" (List [App "UnQual" (List [App
    "Ident" (List [CtorName])]),List []])]))])]),App "BDecls" (List [
    List []])])]])])])]
-- GENERATED STOP
