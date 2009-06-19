
module Data.DeriveDSL(deriveDSL, applyDSL, dynamicDSL) where

import Data.Derive.DSL.Derive
import Data.Derive.DSL.Apply
import Data.Derive.DSL.DSL
import Data.Derive.DSL.HSE
import Data.Derive.DSL.SYB
import Data.Maybe


deriveDSL :: [Decl] -> Maybe DSL
deriveDSL = listToMaybe . derive


applyDSL :: DSL -> Decl -> Maybe [Decl]
applyDSL dsl = fmap (apply dsl) . toInput


dynamicDSL :: DSL -> Maybe [Decl]
dynamicDSL = dslSYB
