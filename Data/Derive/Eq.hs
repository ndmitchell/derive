module Data.Derive.Eq(makeEq) where

import Data.Derive
import Language.Haskell.TH

makeEq = Derivation eq' "Eq"
eq' dat = simple_instance "Eq" dat [funN "==" body]
    where
        body = map rule (dataCtors dat) ++ [defclause 2 false]

rule ctor = sclause [ctp ctor 'a', ctp ctor 'b']
                    (and' (zipWith (==:) (ctv ctor 'a') (ctv ctor 'b')))
