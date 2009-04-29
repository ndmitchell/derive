
module Main where

import HSE
import Apply
import DSL

test = putStr $ unlines $ map prettyPrint $ apply dataTypeList dslEq
