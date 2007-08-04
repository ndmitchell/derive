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
module Data.Derive.Functor(makeFunctor) where

import Language.Haskell.TH.All
import Data.DeriveTraversal


makeFunctor :: Derivation
makeFunctor = derivation derive "Functor"

derive dat = traversalInstance1 functorTraversal "Functor" dat

functorTraversal = defaultTraversalType
        { traversalName   = "fmap"
        , traverseArrow   = functorForArrowType
        , traverseFunc    = \pat rhs -> sclause [vr "f", pat] rhs
        }

functorForArrowType :: Exp -> Exp -> Exp
functorForArrowType a b
  | isId a && isId b  =  id'
  | isId a            =  InfixE Nothing  (l0 ".") (Just b)
  | isId b            =  InfixE (Just a) (l0 ".") Nothing
  | otherwise         =  LamE [l0 "arg"] $ a .: l0 "arg" .: b
 where isId = (id'==)
