{-# LANGUAGE TemplateHaskell #-}

module Test.Data(
    module Test.Data, module Data.DeriveTH
    ) where

import Control.Monad
import Data.DeriveTH
import Language.Haskell.TH.Syntax


derivess :: [Derivation] -> Q [Dec]
derivess = liftM concat . mapM derives


derives :: Derivation -> Q [Dec]
derives f = liftM concat $ mapM (derive f) names


names :: [Name]
names = [''A, ''B, ''Color]




data Color = RGB Int Int Int
           | CMYK Int Int Int Int
           deriving (Show)


data A a = A a [C a]
data B a = B a
type C a = B (B a)
