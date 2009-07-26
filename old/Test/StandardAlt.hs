{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

-- Some definitions have multiple options, this allows the alternative definitions to be tested

module Test.StandardAlt where

import Test.Data


$( derivess [makeEnumCyclic, makeLazySet] )

