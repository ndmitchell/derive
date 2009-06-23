
module Data.Derive.Internal.Derivation(
    Derivation(..), derivationDSL, derivationCustomDSL
    ) where

import Data.DeriveDSL
import Data.Derive.DSL.HSE
import Data.Generics.PlateData


data Derivation = Derivation
    String -- ^ Name
    (FullDataDecl -> Either String [Decl]) -- ^ Operation


derivationDSL :: String -> DSL -> Derivation
derivationDSL name dsl = Derivation name $ applyDSL dsl . snd


derivationCustomDSL :: String -> (FullDataDecl -> Exp -> Exp) -> DSL -> Derivation
derivationCustomDSL name custom dsl = Derivation name $
    \d -> transformBi (f d) $ applyDSL dsl $ snd d
    where
        f d (SpliceExp (ParenSplice x)) = custom d x
        f d x = x

