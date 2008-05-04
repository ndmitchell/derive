{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

-- Would like to make the test suite warning free, but requires tweaks to the instances

module Test.Standard where

import Test.Data

import Data.Generics
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Applicative hiding (Const)
import Prelude hiding (foldr)


$( derivess [makeOrd, makeShow, makeEq, makeBounded, makeEnum, makeRead
            ,makeData, makeTypeable
            ,makeIs, makeHas, makeSet, makeFrom
            ,makeMonoid
            ,makeFunctor
            ] )


$( derivesNot makeFoldable    [''State] )
$( derivesNot makeTraversable [''State] )

-- to prevent errors about missing instances:
instance Eq (a -> b)
instance Show (a -> b)
instance Ord (a -> b)
instance Bounded (a -> b)
instance Read (a -> b)

instance Bounded a => Bounded [a]
