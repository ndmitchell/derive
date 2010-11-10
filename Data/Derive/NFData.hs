module Data.Derive.NFData where
{-
import "deepseq" Control.DeepSeq(NFData, rnf)

example :: Sample
instance NFData a => NFData (Sample a) where
    rnf (First) = ()
    rnf (Second x1 x2) = rnf x1 `seq` rnf x2 `seq` ()
    rnf (Third x1) = rnf x1 `seq` ()

-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeNFData :: Derivation
makeNFData = derivationDSL "NFData" dslNFData

dslNFData =
    List [Instance ["NFData"] "NFData" (List [App "InsDecl" (List [App
    "FunBind" (List [MapCtor (App "Match" (List [App "Ident" (List [
    String "rnf"]),List [App "PParen" (List [App "PApp" (List [App
    "UnQual" (List [App "Ident" (List [CtorName])]),MapField (App
    "PVar" (List [App "Ident" (List [Concat (List [String "x",ShowInt
    FieldIndex])])]))])])],App "Nothing" (List []),App "UnGuardedRhs"
    (List [Fold (App "InfixApp" (List [Head,App "QVarOp" (List [App
    "UnQual" (List [App "Ident" (List [String "seq"])])]),Tail])) (
    Concat (List [MapField (App "App" (List [App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "rnf"])])]),App "Var" (
    List [App "UnQual" (List [App "Ident" (List [Concat (List [String
    "x",ShowInt FieldIndex])])])])])),List [App "Con" (List [App
    "Special" (List [App "UnitCon" (List [])])])]]))]),App "BDecls" (
    List [List []])]))])])])]
-- GENERATED STOP
