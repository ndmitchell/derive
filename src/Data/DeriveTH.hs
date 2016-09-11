-- | The main TH driver module.  It is intended that this need be the
-- only module imported by user code; it takes care of all data
-- threading issues such that all one needs to do is:
--
-- @
--   data Foo = Foo ; $( derive makeEq ''Foo )
-- @
module Data.DeriveTH(derive, derives, deriveFromDec, module Data.Derive.All) where

import Control.Monad

import Data.Derive.All
import Data.Derive.Internal.Derivation
import Language.Haskell.TH.All as TH hiding (Derivation(..),toName)
import Language.Haskell as HS
import Language.Haskell.Convert


-- | Derive an instance of some class. @derive@ only derives instances
-- for the type of the argument.
derive :: Derivation -> TH.Name -> Q [Dec]
derive d name = do
    x <- reify name
    case x of
        TyConI dec -> deriveFromDec d dec
        _ -> error $ "Data.DeriveTH.derive: Expected a data type declaration, got:\n" ++ show x


derives :: [Derivation] -> [TH.Name] -> Q [Dec]
derives xs ys = liftM concat $ sequence [derive x y | y <- ys, x <- xs]


-- | Derive an instance of some class. @deriveFromDec@ only derives instances
-- for the type of the argument.
deriveFromDec :: Derivation -> Dec -> Q [Dec]
deriveFromDec d x = do
    x <- liftM normData $ expandData x
    let unsup x = error $ "Derivation of " ++ derivationName d ++ " does not yet support Template Haskell, requires info for " ++ x
    case derivationOp d (tyCon $ derivationName d) unsup $ toFullDataDecl x of
        Left y -> runIO (putStrLn $ "Warning, couldn't derive: " ++ y) >> return []
        Right v -> return $ convert v

toFullDataDecl :: Dec -> FullDataDecl
toFullDataDecl x = (ModuleName () "Todo", convert x)
