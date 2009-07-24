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
import Data.Traversable
import Control.Applicative(pure, (<*>))

instance Traversable (FailList t1) where
    traverse _f (Zero) = pure Nil
    traverse _f (Fail a1) = pure (Fail a1)
    traverse _f (Const a1 a2) = (Const <$> _f a1) <*> traverse _f a2

instance Traversable Sample where
    traverse _f (First) = pure First
    traverse _f (Second a1 a2) = (Second <$> _f a1) <*> _f a2
    traverse _f (Third a1) = Third <$> _f a1

instance Traversable (Either t1) where
    traverse _f (Left a1) = pure (Left a1)
    traverse _f (Right a1) = Right <$> _f a1
-}

import Data.Derive.Internal.Traversal
import Data.Derive.Internal.Derivation
import Language.Haskell


makeTraversable :: Derivation
makeTraversable = makeTraversableN 1

makeTraversableN :: Int -> Derivation
makeTraversableN n = traversalDerivation1 traverseTraversal{traversalArg = n} "Traversable"

traverseTraversal = defaultTraversalType
        { traversalName  = qname "traverse"
        , traversalId    = var "pure"
        , traversalPlus  = fail "variable used in multiple positions in a data type"
        , traverseTuple  = \args -> liftAN (Con $ Special $ TupleCon Unboxed $ length args) args
        , traverseCtor   = \ctor -> liftAN (con ctor)
        , traverseFunc   = \pat rhs -> Match sl (name "") [pVar "_f", pat] Nothing (UnGuardedRhs rhs) (BDecls [])
        }

liftAN :: Exp -> [Exp] -> Exp
liftAN base args = foldl (<*>) (appP (var "pure") base) args
    where x <*> y = InfixApp (paren x) (QVarOp $ UnQual $ Symbol "<*>") (paren y)
