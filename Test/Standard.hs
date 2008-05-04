{-# LANGUAGE TemplateHaskell #-}

module Test.Standard where

import Test.Data


$( derivess [makeFunctor, makeEq] )
