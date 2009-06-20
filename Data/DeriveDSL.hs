
module Data.DeriveDSL(DSL, deriveDSL, applyDSL, dynamicDSL) where

import Data.Derive.DSL.Derive
import Data.Derive.DSL.Apply
import Data.Derive.DSL.DSL
import Data.Derive.DSL.HSE
import Data.Derive.DSL.SYB
import Data.Maybe


deriveDSL :: [Decl] -> Maybe DSL
deriveDSL = listToMaybe . derive


applyDSL :: DSL -> Decl -> Either String [Decl]
applyDSL dsl inp = case toInput inp of
    Nothing -> Left "Couldn't convert input"
    Just x -> Right $ simplifyOut $ apply dsl x


dynamicDSL :: DSL -> Maybe [Decl]
dynamicDSL = dslSYB
