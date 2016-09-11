{-|
    A pseudo derivation.  For each field in the data type, deriving
    @Ref@ generates @ref@/FieldName/@ = Ref { select = @/fieldName/@ , update =
    \ f v -> v { @/fieldName/@ = f (@/fieldName/@ v) } }@.

    This is intended for use with the compositional functional references
    described in
    <http://www.haskell.org/pipermail/haskell-cafe/2007-June/026477.html>.
-}
module Data.Derive.Ref(makeRef) where

{-
test :: Sample

test :: Computer

refSpeed :: Ref Computer
refSpeed = Ref {select = speed, update = \f v -> v{speed = f (speed v)}}

refWeight :: Ref Computer
refWeight = Ref {select = weight, update = \f v -> v{weight = f (weight v)}}
-}

import Language.Haskell
import Data.Derive.Internal.Derivation


makeRef :: Derivation
makeRef = derivationCustom "Ref" $ \(_,d) -> Right $ concatMap (makeRefField d) $ dataDeclFields d


makeRefField :: DataDecl -> String -> [Decl ()]
makeRefField d field = if isIdent field then [TypeSig () [name ref] typ, bind ref [] bod] else []
    where
        ref = "ref" ++ title field
        typ = TyApp () (tyCon "Ref") (dataDeclType d)

        bod = RecConstr () (qname "Ref")
            [FieldUpdate () (qname "select") (var field)
            ,FieldUpdate () (qname "update") $ Lambda () [pVar "f",pVar "v"] $
                RecUpdate () (var "v") [FieldUpdate () (qname field) $ App () (var "f") $ Paren () $ App () (var field) (var "v")]
            ]
