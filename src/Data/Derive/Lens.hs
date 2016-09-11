{-|
    A pseudo derivation.  For each field in the data type, deriving
    @Lens@ generates @lens@/FieldName/@ = lens @/fieldName/@
    (\ x v -> v { @/fieldName/@ = x })@.

    This works with the @data-lens@ package.
-}
module Data.Derive.Lens(makeLens) where

{-
import "data-lens" Data.Lens.Common

test :: Sample

test :: Computer

lensSpeed :: Lens Computer Int
lensSpeed = lens speed (\x v -> v{speed = x})

lensWeight :: Lens Computer Double
lensWeight = lens weight (\x v -> v{weight = x})
-}

import Language.Haskell
import Data.Derive.Internal.Derivation


makeLens :: Derivation
makeLens = derivationCustom "Lens" $ \(_,d) -> Right $ concatMap (makeLensField d) $ dataDeclFields d


makeLensField :: DataDecl -> String -> [Decl ()]
makeLensField d field = if isIdent field then [TypeSig () [name ref] typ, bind ref [] bod] else []
    where
        ref = "lens" ++ title field
        typ = tyApps (tyCon "Lens") [dataDeclType d, t]
        Just t = lookup field $ concatMap ctorDeclFields $ dataDeclCtors d

        bod = apps (var "lens")
            [var field
            ,Paren () $ Lambda () [pVar "x",pVar "v"] $ RecUpdate () (var "v") [FieldUpdate () (qname field) (var "x")]]
