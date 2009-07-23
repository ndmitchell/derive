{-# LANGUAGE TemplateHaskell #-}


{-
    This module is not written/maintained by the usual Data.Derive author.

    MAINTAINER: Twan van Laarhoven 
    EMAIL: "twanvl" ++ "@" ++ "gmail" ++ "." ++ "com"

    Please send all patches to this module to Neil (ndmitchell -at- gmail),
    and CC Twan.
-}

-- NOTE: Cannot be guessed as it relies on type information

module Data.Derive.Foldable(makeFoldable, makeFoldableN) where

{-
instance Foldable (FailList t1)
    where foldr _f b (Nil) = b
          foldr _f b (Fail a1) = b
          foldr _f b (Const a1 a2) = _f a1 (Data.Foldable.foldr _f b a2)

instance Foldable Sample
    where foldr _f b (First) = b
          foldr _f b (Second a1 a2) = _f a1 (_f a2 b)
          foldr _f b (Third a1) = _f a1 b

instance Foldable (Eitherd t1)
    where foldr _f b (Leftd a1) = b
          foldr _f b (Rightd a1) = _f a1 b
-}

import qualified Data.Foldable ( foldr )

import Language.Haskell.TH.All
import Data.DeriveTraversal


makeFoldable :: Derivation
makeFoldable = makeFoldableN 1

makeFoldableN :: Int -> Derivation
makeFoldableN n = traversalDerivation1 foldrTraversal{traversalArg = n} "Foldable"

foldrTraversal = defaultTraversalType
        { traversalName   = 'Data.Foldable.foldr
        , traversalFunc   = \n a -> l1 "flip" (l1 n a)
        , traversalPlus   = fail "variable used in multiple positions in a data type"
        , traversalId     = l1 "flip" (l0 "const")
        , traverseTuple   =         foldr (.:) id'
        , traverseCtor    = const $ foldr (.:) id'
        , traverseFunc    = \pat rhs -> sclause [vr "_f", vr "b", pat] (AppE rhs (vr "b"))
        }
