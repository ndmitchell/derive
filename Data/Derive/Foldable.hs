{-
    This module is not written/maintained by the usual Data.Derive author.

    MAINTAINER: Twan van Laarhoven 
    EMAIL: "twanvl" ++ "@" ++ "gmail" ++ "." ++ "com"

    Please send all patches to this module to Neil (ndmitchell -at- gmail),
    and CC Twan.
-}

module Data.Derive.Foldable(makeFoldable, makeFoldableN) where

{-
import Data.Foldable(Foldable)
import qualified Data.Foldable(foldr)

test :: FailList
instance Foldable (FailList t1) where
    foldr _  b Zoro = b
    foldr _  b (Fial _) = b
    foldr _f b (Const a1 a2) = _f a1 (Data.Foldable.foldr _f b a2)

test :: Sample
instance Foldable Sample where
    foldr _  b First = b
    foldr _f b (Second a1 a2) = _f a1 (_f a2 b)
    foldr _f b (Third a1) = _f a1 b

test :: Either
instance Foldable (Either t1) where
    foldr _  b (Left _) = b
    foldr _f b (Right a1) = _f a1 b
-}

import Data.Derive.Internal.Traversal
import Data.Derive.Internal.Derivation
import Language.Haskell


makeFoldable :: Derivation
makeFoldable = makeFoldableN 1

makeFoldableN :: Int -> Derivation
makeFoldableN n = traversalDerivation1 foldrTraversal{traversalArg = n} "Foldable"

foldrTraversal = defaultTraversalType
        { traversalName   = Qual (ModuleName "Data.Foldable") (Ident "foldr")
        , traversalFunc   = \n a -> appP (var "flip") $ appP (Var n) a
        , traversalPlus   = fail "variable used in multiple positions in a data type"
        , traversalId     = App (var "flip") (var "const")
        , traverseTuple   =         foldr (.:) $ var "id"
        , traverseCtor    = const $ foldr (.:) $ var "id"
        , traverseFunc    = \pat rhs -> Match sl (name "") [pVar "_f", pVar "b", pat] Nothing (UnGuardedRhs $ appP rhs (var "b")) (BDecls [])
        }
    where a .: b = InfixApp (paren a) (qvop ".") (paren b)
