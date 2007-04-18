-- NOTE: Cannot be guessed as it inducts on the data type (not its constructors)

module Data.Derive.TTypeable(makeTTypeable) where

import Language.Haskell.TH.All
import Data.Char
import Data.Bits

makeTTypeable = Derivation ttypeable' "TTypeable"
ttypeable' dat = [InstanceD ctx hd []]
    where
        bits x = [ if x .&. mask /= 0 then hSucc' hZero' else hZero' | mask <- [1,2,4,8,16,32,64,128] ]
        bitnam = foldr hCons' hNil' (concatMap (bits . ord) (dataName dat))

        hd = l2 "TTypeable" (lK (dataName dat) (vrs 't')) (foldr hCons' hNil' (bitnam : vrs 'n'))
        ctx = zipWith (l2 "TTypeable") (vrs 't') (vrs 'n')

        vrs = flip vars (dataArity dat)
