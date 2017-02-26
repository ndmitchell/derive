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
    List [Instance ["NFData"] "NFData" (App "Just" (List [List [App
    "InsDecl" (List [App "()" (List []),App "FunBind" (List [App "()"
    (List []),MapCtor (App "Match" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "rnf"]),List [App
    "PParen" (List [App "()" (List []),App "PApp" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),CtorName])]),MapField (App "PVar" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),Concat (List
    [String "x",ShowInt FieldIndex])])]))])])],App "UnGuardedRhs" (
    List [App "()" (List []),Fold (App "InfixApp" (List [App "()" (
    List []),Head,App "QVarOp" (List [App "()" (List []),App "UnQual"
    (List [App "()" (List []),App "Ident" (List [App "()" (List []),
    String "seq"])])]),Tail])) (Concat (List [MapField (App "App" (
    List [App "()" (List []),App "Var" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "rnf"])])]),App "Var" (List [App "()" (List []),
    App "UnQual" (List [App "()" (List []),App "Ident" (List [App "()"
    (List []),Concat (List [String "x",ShowInt FieldIndex])])])])])),
    List [App "Con" (List [App "()" (List []),App "Special" (List [App
    "()" (List []),App "UnitCon" (List [App "()" (List [])])])])]]))])
    ,App "Nothing" (List [])]))])])]]))]
-- GENERATED STOP
