{-# LANGUAGE TemplateHaskell #-}

module Test.UpdateIs where

import Test.Data
import Data.DeriveTH

data PolyRecord i = PR { pr1 :: i,
                         pr2 :: [i] }

$( derive makeUpdate ''Computer )
$( derive makeUpdate ''PolyRecord )
$( derive makeIs ''Computer )
$( derive makeIs ''PolyRecord )
$( derives makeFrom )
$( derive makeFrom ''PolyRecord )
