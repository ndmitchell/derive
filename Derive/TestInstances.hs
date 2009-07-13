{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -w #-}

module Derive.TestInstances where

import Data.Derive.Class.Default
import Data.Monoid

instance Bounded Double
instance Bounded String
instance Default Double
instance Default Int
instance Default String
instance Monoid Double
instance Monoid Int
