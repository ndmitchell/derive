{-# LANGUAGE TemplateHaskell #-}

module Test.All where

import Data.DeriveTH
import Data.Derive.Eq
import Data.Derive.Binary


data Color = RGB Int Int Int
           | CMYK Int Int Int Int
           deriving (Show)

$( derive makeEq ''Color )
