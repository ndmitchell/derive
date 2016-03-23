{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- Needed for Haddock docs

-- | Use this module to create your own derive program, supporting custom derivations.
--   As an example:
--
-- @
--   import "Data.DeriveMain"        ('deriveMain')
--   import "Data.Derive.All"        ('derivations')
--   import MyProject.MyDerivation (myDerivation)
-- @
--
-- @
--   main :: IO
--   main = 'deriveMain' $ [myDerivation] ++ 'derivations'
-- @
module Data.DeriveMain(deriveMain) where

import Derive.Main
import Data.Derive.All(derivations)
