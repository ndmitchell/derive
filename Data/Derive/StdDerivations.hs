{-# OPTIONS_GHC -fglasgow-exts #-}
-- | This module re-exports all of the standard derivations for
-- convenience, and also provides a mapping from Strings to
-- derivation functions (so as to facilitate the writing of the
-- DrIFT-workalike driver).
module Data.Derive.StdDerivations
       (derivable,
        module Data.Derive.Eq,
        module Data.Derive.Functor,
        module Data.Derive.Binary,
        module Data.Derive.BinaryDefer
       ) where

import Data.Derive.Eq
import Data.Derive.Functor
import Data.Derive.Binary
import Data.Derive.BinaryDefer

-- | A default set of standard derivations.
derivable = [ eq, functor, binary, binarydefer ]
