{-# OPTIONS_GHC -fglasgow-exts #-}
-- | This module contains the common table of derivers; it is
-- imported by all the driver modules.
module Data.Derive.AllDerivers
       (derivable, getDeriver) where

import Data.Derive
import Data.Maybe

import qualified Data.Derive.Eq
import qualified Data.Derive.BinaryDefer
import qualified Data.Derive.Binary
import qualified Data.Derive.Functor

-- | List of things we are able to derive.
derivable :: [String]
derivable = map fst derivers

-- | The main lookup table mapping class names to functions able to
-- achieve derivation.
derivers = [("Eq",Data.Derive.Eq.derive)
           ,("BinaryDefer",Data.Derive.BinaryDefer.derive)
           ,("Binary",Data.Derive.Binary.derive)
           ,("Functor",Data.Derive.Functor.derive)
           ]

-- | Map a class name to a derivation function.
getDeriver :: String -> (DataDef -> [String])
getDeriver x = fromMaybe (error $ "Do not know how to derive " ++ x) (lookup x derivers)
