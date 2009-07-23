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

{-
test :: FailList
instance Functor (FailList t1) where
    fmap _f Zero = Zero
    fmap _f (Fail a1) = Fail a1
    fmap _f (Const a1 a2) = Const (_f a1) (fmap _f a2)

test :: State
instance Functor (State t1) where
    fmap _f (StateT a1) = StateT (fmap _f . a1)

test :: Sample
instance Functor Sample where
    fmap _f First = First
    fmap _f (Second a1 a2) = Second (_f a1) (_f a2)
    fmap _f (Third a1) = Third (_f a1)
-}


import Data.Derive.Internal.Traversal
import Data.Derive.Internal.Derivation
import Language.Haskell


makeFunctor :: Derivation
makeFunctor = makeFunctorN 1

makeFunctorN :: Int -> Derivation
makeFunctorN n = traversalDerivation1 functorTraversal{traversalArg = n} "Functor"

functorTraversal = defaultTraversalType
        { traversalName   = qname "fmap"
        , traverseArrow   = Just functorForArrowType
        , traverseFunc    = \pat rhs -> Match sl (name "") [pVar "_f", pat] Nothing (UnGuardedRhs rhs) (BDecls [])
        }

functorForArrowType :: Exp -> Exp -> Exp
functorForArrowType a b
  | isId a && isId b  =  var "id"
  | isId a            =  LeftSection b (qvop ".")
  | isId b            =  RightSection (qvop ".") a
  | otherwise         =  Lambda sl [pVar "arg"] $ b .: var "arg" .: a
 where isId = (var "id" ==)
       a .: b = InfixApp (paren a) (qvop ".") (paren b)
