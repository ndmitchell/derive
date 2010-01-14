{-|
    A pseudo derivation.  For each field in the data type, deriving
    @LazySet@ generates a function like a record updator, but lazy where possible.
    This is very useful in certain situations to improve laziness
    properties.  A setter is only lazy if that field is present in one constructor.
-}
module Data.Derive.LazySet(makeLazySet) where
{-

test :: Computer

setSpeed :: Int -> Computer -> Computer
setSpeed v x = x{speed=v}

setWeight :: Double -> Computer -> Computer
setWeight v x = Laptop v (speed x)

test :: Sample

-}

import Language.Haskell
import Data.Derive.Internal.Derivation


makeLazySet :: Derivation
makeLazySet = derivationCustom "LazySet" $ \(_,d) -> Right $ concatMap (makeLazySetField d) $ dataDeclFields d


makeLazySetField :: DataDecl -> String -> [Decl]
makeLazySetField d field = [TypeSig sl [name fun] typ, bind fun [pVar "v",pVar "x"] bod]
    where
        fun = "set" ++ title field
        typ = t `TyFun` (dataDeclType d `TyFun` dataDeclType d)
        (t,c):tc = [(fromBangType t,c) | c <- dataDeclCtors d, (n,t) <- ctorDeclFields c, n == field]

        bod | null tc = apps (con $ ctorDeclName c) [n == field ? var "v" $ Paren $ App (var n) (var "x") | (n,t) <- ctorDeclFields c]
            | otherwise = RecUpdate (var "x") [FieldUpdate (qname field) (var "v")]
