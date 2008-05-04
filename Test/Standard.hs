{-# LANGUAGE TemplateHaskell #-}

module Test.Standard where

import Test.Data


$( derivess [makeOrd, makeShow, makeEq] )


$( derivesNot makeFunctor [''State] )

instance Eq (a -> b)
instance Show (a -> b)
instance Ord (a -> b)
