{-
    This module is not written/maintained by the usual Data.Derive author.

    MAINTAINER: Twan van Laarhoven 
    EMAIL: "twanvl" ++ "@" ++ "gmail" ++ "." ++ "com"

    Please send all patches to this module to Neil (ndmitchell -at- gmail),
    and CC Twan.
-}

-- NOTE: Cannot be guessed as it relies on type information

-- | Derives 'Functor', as discussed on the Haskell-prime mailing list:
-- <http://www.mail-archive.com/haskell-prime@haskell.org/msg02116.html>.
module Data.Derive.Functor(makeFunctor,makeFunctorN) where

import Language.Haskell.TH.All
import Data.DeriveTraversal


makeFunctor :: Derivation
makeFunctor = makeFunctorN 1

makeFunctorN :: Int -> Derivation
makeFunctorN n = traversalDerivation1 functorTraversal{traversalArg = n} "Functor"

functorTraversal = defaultTraversalType
        { traversalName   = "fmap"
        , traverseArrow   = functorForArrowType
        , traverseFunc    = \pat rhs -> sclause [vr "_f", pat] rhs
        }

functorForArrowType :: Exp -> Exp -> Exp
functorForArrowType a b
  | isId a && isId b  =  id'
  | isId a            =  InfixE (Just b) (l0 ".") Nothing
  | isId b            =  InfixE Nothing  (l0 ".") (Just a)
  | otherwise         =  LamE [l0 "arg"] $ b .: l0 "arg" .: a
 where isId = (id'==)
