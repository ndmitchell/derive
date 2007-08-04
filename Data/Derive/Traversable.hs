{-
    This module is not written/maintained by the usual Data.Derive author.

    MAINTAINER: Twan van Laarhoven 
    EMAIL: "twanvl" ++ "@" ++ "gmail" ++ "." ++ "com"

    Please send all patches to this module to Neil (ndmitchell -at- gmail),
    and CC Twan.
-}

-- NOTE: Cannot be guessed as it relies on type information

module Data.Derive.Traversable(makeTraversable) where

import Language.Haskell.TH.All
import Data.DeriveTraversal


makeTraversable :: Derivation
makeTraversable = derivation derive "Traversable"

derive dat = traversalInstance1 traverseTraversal "Traversable" dat

traverseTraversal = defaultTraversalType
        { traversalName  = "traverse"
        , traversalId    = l0 "pure"
        , traversalPlus  = fail "variable used in multiple positions in a data type"
        , traverseTuple  = \args -> liftAN (VarE $ tupleDataName $ length args) args
        , traverseCtor   = \ctor -> liftAN (l0 ctor)
        , traverseFunc   = \pat rhs -> sclause [vr "f", pat] rhs
        }

liftAN :: Exp -> [Exp] -> Exp
liftAN base args = foldl (l2 "<*>") (l1 "pure" base) args
