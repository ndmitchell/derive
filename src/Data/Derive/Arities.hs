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
    List [Instance [] "Arities" (App "Just" (List [List [App "InsDecl"
    (List [App "()" (List []),App "FunBind" (List [App "()" (List []),
    List [App "Match" (List [App "()" (List []),App "Ident" (List [App
    "()" (List []),String "arities"]),List [App "PWildCard" (List [App
    "()" (List [])])],App "UnGuardedRhs" (List [App "()" (List []),App
    "List" (List [App "()" (List []),MapCtor (Application (List [App
    "Var" (List [App "()" (List []),App "UnQual" (List [App "()" (List
    []),App "Ident" (List [App "()" (List []),String "const"])])]),App
    "Lit" (List [App "()" (List []),App "Int" (List [App "()" (List []
    ),CtorArity,ShowInt CtorArity])]),App "RecConstr" (List [App "()"
    (List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),CtorName])]),List []])]))])]),App "Nothing" (
    List [])])]])])]]))]
-- GENERATED STOP
