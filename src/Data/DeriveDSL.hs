
module Data.DeriveDSL(DSL, deriveDSL, applyDSL, dynamicDSL) where

import Data.Derive.DSL.Derive
import Data.Derive.DSL.Apply
import Data.Derive.DSL.DSL
import Data.Derive.DSL.HSE
import Data.Derive.DSL.SYB
import Data.Maybe


deriveDSL :: [Decl ()] -> Maybe DSL
deriveDSL = listToMaybe . derive


applyDSL :: DSL -> DataDecl -> Either String [Decl ()]
applyDSL dsl inp = Right $ apply dsl $ toInput inp


dynamicDSL :: DSL -> Maybe [Decl ()]
dynamicDSL = dslSYB
