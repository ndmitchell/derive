
module Language.Haskell.TH.All (
    module Language.Haskell.TH.All,
    module Language.Haskell.TH.Syntax, module Language.Haskell.TH.Peephole,
    module Language.Haskell.TH.Helper, module Language.Haskell.TH.FixedPpr,
    module Language.Haskell.TH.Data
    ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Peephole
import Language.Haskell.TH.Helper
import Language.Haskell.TH.FixedPpr
import Language.Haskell.TH.Data



-- | The type of ways to derive classes.
--   Should not really be in this module!
data Derivation = Derivation {
      derivationDeriver :: DataDef -> [Dec], -- ^ The derivation function proper
      derivationName    :: String            -- ^ The name of the derivation
    }
