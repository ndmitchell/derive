
module Main where

import HSE
import Apply
import DSL
import Guess

test = do
    let res = apply dataTypeCtors dslEq
    let dsl = head $ guess res
    putStr $ showRes res
    print dsl
    putStr $ showRes $ apply dataTypeList dsl

    
