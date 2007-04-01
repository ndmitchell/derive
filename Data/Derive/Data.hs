module Data.Derive.Data(makeData) where

import Data.Derive
import Language.Haskell.TH

makeData = Derivation data' "Data"
data' dat = simple_instance "Data" dat [funN "gfoldl" body]
    where
        body = map f $ dataCtors dat
        
        f ctor = sclause [vr "k",vr "z",ctp ctor 'x']
                         (foldl f (AppE (vr "z") (l0 (ctorName ctor))) (ctv ctor 'x'))
            where
                f x y = AppE (AppE (vr "k") x) y
