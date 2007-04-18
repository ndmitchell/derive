-- NOTE: Cannot be guessed as it inducts on the data type (not its constructors)

module Data.Derive.TTypeable(makeTTypeable) where

import Language.Haskell.TH.All
import Data.Char
import Data.Bits

makeTTypeable = Derivation ttypeable' "TTypeable"
ttypeable' dat = [InstanceD ctx hd []]
    where
        l0t = ConT . mkName
        l1t n x = AppT (l0t n) x
        l2t n x y = AppT (l1t n x) y
        lkt n l = foldl AppT (l0t n) l

        hCons' = l2t "HCons"
        hNil'  = l0t "HNil"

        hSucc' = l1t "HSucc"
        hZero' = l0t "HZero"

        bits x = [ if x .&. mask /= 0 then hSucc' hZero' else hZero' | mask <- [1,2,4,8,16,32,64,128] ]
        bitnam = foldr hCons' hNil' (concatMap (bits . ord) (dataName dat))

        hd = l2t "TTypeable" (lkt (dataName dat) (vars 't')) (foldr hCons' hNil' (bitnam : vars 'n'))
        ctx = zipWith (l2t "TTypeable") (vars 't') (vars 'n')

        vars ch = map (VarT . mkName . (ch:) . show) [ 1 .. dataArity dat ]
