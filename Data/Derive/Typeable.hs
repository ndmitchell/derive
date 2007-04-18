-- NOTE: Cannot be guessed as it inducts on the data type (not its constructors)

module Data.Derive.Typeable(makeTypeable) where

import Language.Haskell.TH.All
import Data.Char

makeTypeable = Derivation typeable' "Typeable"
typeable' dat = [funN nam [sclause [] (l1 "mkTyCon" $ lit $ dataName dat)]
                ,InstanceD [] hd [def]]
    where
        nam = [if x == '.' then '_' else x | x <- "typename_" ++ dataName dat]

        n = if dataArity dat == 0 then "" else show (dataArity dat)
        hd = l1 ("Typeable" ++ n) (l0 (dataName dat))
        def = funN ("typeOf" ++ n) [defclause 1 (l2 "mkTyConApp" (vr nam) nil)]

