{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -w #-}

module Derive.TestInstances where

import Data.Derive.Class.Default
import Data.Monoid
import Data.Binary
import Control.Parallel.Strategies

instance Bounded Double
instance Bounded a => Bounded [a]
instance Default Double
instance Default Int
instance Default [a]
instance Monoid Double
instance Monoid Int

instance Show (a -> b)
instance Default (a -> b)
instance NFData (a -> b)
instance Ord (a -> b)
instance Bounded (a -> b)
instance Binary (a -> b)
instance Eq (a -> b)
instance Read (a -> b)
