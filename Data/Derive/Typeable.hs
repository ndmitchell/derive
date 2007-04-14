-- NOTE: Cannot be guessed as it inducts on the data type (not its constructors)

module Data.Derive.Typeable(makeTypeable) where

import Language.Haskell.TH.All
import Data.Char

makeTypeable = Derivation typeable' "Typeable"
typeable' dat = [FunD nam [sclause [] (l1 "mkTyCon" $ LitE $ StringL $ dataName dat)]
                ,InstanceD [] hd [def]]
    where
        nam = mkName [if x == '.' then '_' else x | x <- "typename_" ++ dataName dat]
    
        n = if dataArity dat == 0 then "" else show (dataArity dat)
        hd = ConT (mkName $ "Typeable" ++ n) `AppT` ConT (mkName (dataName dat))
        def = funN ("typeOf" ++ n) [sclause [WildP] (l2 "mkTyConApp" (VarE nam) nil)]

