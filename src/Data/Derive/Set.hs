{-|
    A pseudo derivation.  For each field in the data type, deriving
    @Set@ generates @set@/FieldName/@ v x = x{@/fieldName/@ = v}@.
    This derivation is intended to work around the fact that in Haskell
    assigning to a field is not a first class object (although
    extracting from a field is).
-}
module Data.Derive.Set(makeSet) where

{-

test :: Computer

setSpeed :: Int -> Computer -> Computer
setSpeed v x = x{speed=v}

setWeight :: Double -> Computer -> Computer
setWeight v x = x{weight=v}

test :: Sample

-}

import Language.Haskell
import Data.Derive.Internal.Derivation
import Data.Maybe


makeSet :: Derivation
makeSet = derivationCustom "Set" $ \(_,d) -> Right $ concatMap (makeSetField d) $ dataDeclFields d


makeSetField :: DataDecl -> String -> [Decl ()]
makeSetField d field = [TypeSig () [name set] typ, bind set [pVar "v",pVar "x"] bod]
    where
        set = "set" ++ title field
        tyFun = TyFun ()
        typ = typField `tyFun` (dataDeclType d `tyFun` dataDeclType d)
        typField = fromJust $ lookup field $ concatMap ctorDeclFields $ dataDeclCtors d

        bod = RecUpdate () (var "x") [FieldUpdate () (qname field) (var "v")]
