{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -w #-}

module Derive.TestInstances where

import Data.Derive.Class.Default
import Data.Monoid
import Data.Binary
import Data.Serialize
import Data.Binary.Defer
import Control.DeepSeq
import Data.Generics.Uniplate.Typeable
import Text.JSON

instance Bounded Double
instance Bounded a => Bounded [a]
instance Default Double
instance Default Int
instance Default [a]
instance Monoid Double
instance Monoid Int
instance BinaryDefer Double

instance Show (a -> b)
instance Default (a -> b)
instance Ord (a -> b)
instance Bounded (a -> b)
instance Binary (a -> b)
instance Serialize (a -> b)
instance Eq (a -> b)
instance Read (a -> b)
instance BinaryDefer (a -> b)
instance JSON (a -> b)

instance PlateAll (a -> (a,b)) c
