
module Data.DeriveDSL(DSL, deriveDSL, applyDSL, dynamicDSL, impureDSL) where

import Data.Derive.DSL.Derive
import Data.Derive.DSL.Apply
import Data.Derive.DSL.DSL
import Data.Derive.DSL.HSE
import Data.Derive.DSL.SYB
import Data.Maybe
import Data.Generics.PlateData


deriveDSL :: [Decl] -> Maybe DSL
deriveDSL = listToMaybe . derive


applyDSL :: DSL -> DataDecl -> Either String [Decl]
applyDSL dsl inp = Right $ simplify $ apply dsl $ toInput inp


dynamicDSL :: DSL -> Maybe [Decl]
dynamicDSL = dslSYB


impureDSL :: DSL -> Bool
impureDSL dsl = or [True | App "SpliceExp" _ <- universe dsl]
