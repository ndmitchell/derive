
module Data.Derive.Internal.Derivation(
    Derivation(..), derivationDSL, derivationCustomDSL,
    customSplice, customContext
    ) where

import Data.DeriveDSL
import Data.Derive.DSL.HSE
import Data.Generics.PlateData


data Derivation = Derivation
    {derivationName :: String
    ,derivationOp :: FullDataDecl -> Either String [Decl]
    }


derivationDSL :: String -> DSL -> Derivation
derivationDSL name dsl = Derivation name $ applyDSL dsl . snd


derivationCustomDSL :: String -> (FullDataDecl -> [Decl] -> [Decl]) -> DSL -> Derivation
derivationCustomDSL name custom dsl = Derivation name $
    \d -> case applyDSL dsl $ snd d of
              Left x -> Left x
              Right x -> Right $ custom d x


customSplice :: (FullDataDecl -> Exp -> Exp) -> (FullDataDecl -> [Decl] -> [Decl])
customSplice custom d = transformBi f
    where
        f (SpliceExp (ParenSplice x)) = custom d x
        f x = x


customContext :: (FullDataDecl -> Context) -> (FullDataDecl -> [Decl] -> [Decl])
customContext custom d = map f
    where
        ctx = custom d
        f (InstDecl sl _ a b c) = InstDecl sl  ctx a b c
        f x = x
