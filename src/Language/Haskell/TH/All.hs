{-# OPTIONS_GHC -w #-}

module Language.Haskell.TH.All (
    module Language.Haskell.TH.All,
    module Language.Haskell.TH.Syntax, module Language.Haskell.TH.Peephole,
    module Language.Haskell.TH.Helper,
    module Language.Haskell.TH.Data,   module Language.Haskell.TH.ExpandSynonym,
    ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Peephole
import Language.Haskell.TH.Helper
import Language.Haskell.TH.ExpandSynonym
import Language.Haskell.TH.Data

import Control.Monad



-- | The type of ways to derive classes.
--   Should not really be in this module!
data Derivation = Derivation {
      derivationDeriver :: DataDef -> Q [Dec], -- ^ The derivation function proper
      derivationName    :: String              -- ^ The name of the derivation
    }


-- create a new derivation more abstractly
derivation :: (DataDef -> [Dec]) -> String -> Derivation
derivation f = Derivation (return . f)


derivationQ :: (DataDef -> Q [Dec]) -> String -> Derivation
derivationQ = Derivation
