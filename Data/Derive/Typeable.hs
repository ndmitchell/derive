module Data.Derive.Typeable(makeTypeable) where

import Language.Haskell.TH.All
import Data.Char

makeTypeable = Derivation typeable' "Typeable"
typeable' dat = [FunD nam [sclause [] (LitE $ StringL $ dataName dat)]
                ,InstanceD [] hd [def]]
    where
        nam = mkName [if x == '.' then '_' else x | x <- "typename_" ++ dataName dat]
    
        n = if dataArity dat == 0 then "" else show (dataArity dat)
        hd = ConT (mkName $ "Typeable" ++ n) `AppT` ConT (mkName (dataName dat))
        def = funN ("typeOf" ++ n) [sclause [WildP] (VarE nam)]

