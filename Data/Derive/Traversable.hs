{-
    This module is not written/maintained by the usual Data.Derive author.

    MAINTAINER: Twan van Laarhoven 
    EMAIL: "twanvl" ++ "@" ++ "gmail" ++ "." ++ "com"

    Please send all patches to this module to Neil (ndmitchell -at- gmail),
    and CC Twan.
-}

-- NOTE: Cannot be guessed as it relies on type information

module Data.Derive.Traversable(makeTraversable, makeTraversableN) where

{-
instance Traversable (FailList t1)
    where traverse _f (Nil) = pure Nil
          traverse _f (Fail a1) = pure (Fail a1)
          traverse _f (Const a1 a2) = (Const <$> _f a1) <*> traverse _f a2

instance Traversable Sample
    where traverse _f (First) = pure First
          traverse _f (Second a1 a2) = (Second <$> _f a1) <*> _f a2
          traverse _f (Third a1) = Third <$> _f a1

instance Traversable (Eitherd t1)
    where traverse _f (Leftd a1) = pure (Leftd a1)
          traverse _f (Rightd a1) = Rightd <$> _f a1
-}

import Language.Haskell.TH.All
import Data.DeriveTraversal


makeTraversable :: Derivation
makeTraversable = makeTraversableN 1

makeTraversableN :: Int -> Derivation
makeTraversableN n = traversalDerivation1 traverseTraversal{traversalArg = n} "Traversable"

traverseTraversal = defaultTraversalType
        { traversalName  = "traverse"
        , traversalId    = l0 "pure"
        , traversalPlus  = fail "variable used in multiple positions in a data type"
        , traverseTuple  = \args -> liftAN (ConE $ tupleDataName $ length args) args
        , traverseCtor   = \ctor -> liftAN (l0 ctor)
        , traverseFunc   = \pat rhs -> sclause [vr "_f", pat] rhs
        }

liftAN :: Exp -> [Exp] -> Exp
liftAN base args = foldl (l2 "<*>") (l1 "pure" base) args
