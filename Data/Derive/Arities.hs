{-
import {- "derive" -} Data.Derive.Class.Arities

{-# EXAMPLE #-}

instance Arities (Sample a) where
    arities _ = [const 0 First{}, const 2 Second{}, const 1 Third{}]

{-# TEST [] #-}

instance Arities [a] where
    arities _ = [0,2]

{-# TEST Bool #-}

instance Arities Bool where
    arities _ = [0,0]

{-# TEST Either #-}

instance Arities (Either a b) where
    arities _ = [1,1]

-}

-- GENERATED START

module Data.Derive.Arities where

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

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

makeArities :: Derivation
makeArities = derivationDSL "Arities" dslArities

-- GENERATED STOP
