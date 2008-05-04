{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

-- Would like to make the test suite warning free, but requires tweaks to the instances

module Test.Standard where

import Test.Data

import Data.Generics
import Data.Monoid


$( derivess [makeOrd, makeShow, makeEq, makeBounded, makeEnum, makeRead
            ,makeData, makeTypeable
            ,makeIs, makeHas, makeSet, makeFrom
            ,makeMonoid
            ] )


$( derivesNot makeFunctor [''State] )

instance Eq (a -> b)
instance Show (a -> b)
instance Ord (a -> b)
instance Bounded (a -> b)
instance Read (a -> b)

instance Bounded a => Bounded [a]
