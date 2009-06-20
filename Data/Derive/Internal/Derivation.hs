
module Data.Derive.Internal.Derivation(
    Derivation(..), derivationDSL, derivation
    ) where

import Data.DeriveDSL
import Data.Derive.DSL.HSE


data Derivation = Derivation
    {derivationName :: String
    ,derivationDeriver :: Decl -> Either String [Decl]
    }


derivation :: String -> (Decl -> Either String [Decl]) -> Derivation
derivation = Derivation

derivationDSL :: String -> DSL -> Derivation
derivationDSL name dsl = derivation name $ applyDSL dsl

