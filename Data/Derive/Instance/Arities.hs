-- GENERATED START
{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}

module Data.Derive.Instance.Arities where

import Data.Derive.Class.Arities
import Data.Derive.Internal.Instance

instance Data d_type => Arities d_type where
        arities _
          = [const (d_ctorArity d_ctor) (d_ctorValue d_ctor :: d_type) |
             d_ctor <- d_dataCtors (undefined :: d_type)]

-- GENERATED STOP
