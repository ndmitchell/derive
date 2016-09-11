{-|
    Has is a pseudo derivation.  For each field of any constructor of
    the data type, Has generates @has@/FieldName/ which returns 'True'
    if given the the given field is a member of the constructor of the
    passed object, and 'False' otherwise.
-}
module Data.Derive.Has(makeHas) where

{-
test :: Computer

hasSpeed :: Computer -> Bool
hasSpeed _ = True

hasWeight :: Computer -> Bool
hasWeight Laptop{} = True
hasWeight _ = False

test :: Sample
-}

import Language.Haskell
import Data.Derive.Internal.Derivation
import Data.List


makeHas :: Derivation
makeHas = derivationCustom "Has" $ \(_,d) -> Right $ concatMap (makeHasField d) $ dataDeclFields d


makeHasField :: DataDecl -> String -> [Decl ()]
makeHasField d field = if isIdent field then [TypeSig () [name has] typ, binds has ms] else []
    where
        has = "has" ++ title field
        typ = TyFun () (dataDeclType d) (tyCon "Bool")
        (yes,no) = partition (elem field . map fst . ctorDeclFields) $ dataDeclCtors d
        match pat val = ([pat], con val)

        ms | null no = [match (PWildCard ()) "True"]
           | otherwise = [match (PRec () (qname $ ctorDeclName c) []) "True" | c <- yes] ++ [match (PWildCard ()) "False"]
