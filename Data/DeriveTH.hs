-- | The main TH driver module.  It is intended that this need be the
-- only module imported by user code; it takes care of all data
-- threading issues such that all one needs to do is:
--
-- @
--   data Foo = Foo ; $( derive makeEq ''Foo )
-- @
module Data.DeriveTH(derive, deriveFromDec, module Data.Derive.All) where

import Data.List
import Control.Monad

import Data.Derive.All
import Language.Haskell.TH.All hiding (Derivation)


-- | Derive an instance of some class. @derive@ only derives instances
-- for the type of the argument.
derive :: Derivation -> Name -> Q [Dec]
derive d name = do
    x <- reify name
    case x of
        TyConI dec -> deriveFromDec d dec
        _ -> error $ "Data.DeriveTH.derive: Expected a data type declaration, got:\n" ++ show x

-- | Derive an instance of some class. @deriveFromDec@ only derives instances
-- for the type of the argument.
deriveFromDec :: Derivation -> Dec -> Q [Dec]
deriveFromDec d x = do
    x <- liftM normData $ expandData x
    return []

