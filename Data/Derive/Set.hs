{-|
    A pseudo derivation.  For each field in the data type, deriving
    @Set@ generates @set@/FieldName/@ v x = x{@/fieldName/@ = v}@.
    This derivation is intended to work around the fact that in Haskell
    assigning to a field is not a first class object (although
    extracting from a field is).
-}
module Data.Derive.Set(makeSet) where

{-

{-# TEST Computer #-}

setSpeed :: Int -> Computer -> Computer
setSpeed v x = x{speed=v}

setWeight :: Double -> Computer -> Computer
setWeight v x = x{weight=v}

{-# TEST Sample #-}

-}

import Language.Haskell
import Data.Derive.Internal.Derivation
import Data.Maybe


makeSet :: Derivation
makeSet = Derivation "Set" $ \(_,d) -> Right $ concatMap (makeSetField d) $ dataDeclFields d


makeSetField :: DataDecl -> String -> [Decl]
makeSetField d field = [TypeSig sl [name set] typ, FunBind [m]]
    where
        set = "set" ++ title field
        typ = typField `TyFun` (dataDeclType d `TyFun` dataDeclType d)
        typField = fromBangType $ fromJust $ lookup field $ concatMap ctorDeclFields $ dataDeclCtors d

        m = Match sl (name set) [pVar "v",pVar "x"] Nothing (UnGuardedRhs bod) (BDecls [])
        bod = RecUpdate (var "x") [FieldUpdate (qname field) (var "v")]
