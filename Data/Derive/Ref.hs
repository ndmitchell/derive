
-- | A pseudo derivation.  For each field in the data type, deriving
-- @Ref@ generates @ref@/FieldName/@ = Ref { select = @/fieldName/@ , update =
-- \ f v -> v { @/fieldName/@ = f (@/fieldName/@ v) } }@.
--
-- This is intended for use with the compositional functional references
-- described in
-- <http://www.haskell.org/pipermail/haskell-cafe/2007-June/026477.html>.
module Data.Derive.Ref(makeRef) where

import Language.Haskell.TH.All
import Data.Char
import Data.List

makeRef :: Derivation
makeRef = derivation ref' "Ref"

ref' dat = map f fields
    where
        ctors = dataCtors dat
        fields = nub $ concatMap ctorFields ctors
        
        f field = funN ("ref" ++ toUpper (head field) : tail field) $
                       [sclause [] $ l2 "Ref" (vr field)
                                              (LamE [vr "f",vr "v"] (RecUpdE (vr "v") [(mkName field, l1 "f" (l1 field (vr "v")))]))]
