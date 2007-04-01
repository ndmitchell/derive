module Data.Derive.Ord(makeOrd) where

import Data.Derive
import Language.Haskell.TH

makeOrd = Derivation ord' "Ord"
ord' dat = simple_instance "Ord" dat [funN "compare" body]
    where
        obs  = zip [0..] (dataCtors dat)
        body = [ sclause [ctp (snd x) 'a', ctp (snd y) 'b'] (rule x y)
                     | x <- obs , y <- obs ]

rule (i1,c1) (i2,c2) | i1 < i2   = l0 "LT"
                     | i1 > i2   = l0 "GT"
                     | otherwise = l2 "compare" (tup2 c1 'a') (tup2 c2 'b')

tup2 c ch = foldr (l2 "(,)") (lit ()) (ctv c ch)
