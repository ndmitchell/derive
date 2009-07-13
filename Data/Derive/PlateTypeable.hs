module Data.Derive.PlateTypeable where
{-
import "uniplate" Data.Generics.PlateTypeable

example :: Sample

instance (PlateAll a (Sample a), Typeable a) => Uniplate (Sample a) where
    uniplate = uniplateAll

instance (Typeable t, Typeable a, Uniplate t, PlateAll a t) => PlateAll (Sample a) t where
    plateAll (First) = plate First
    plateAll (Second x1 x2)  = plate Second |+ x1 |+ x2
    plateAll (Third x1) = plate Third |+ x1

-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makePlateTypeable :: Derivation
makePlateTypeable = derivationDSL "PlateTypeable" dslPlateTypeable

dslPlateTypeable =
    List [Instance ["PlateAll","Typeable"] "Uniplate" (List [App
    "InsDecl" (List [App "PatBind" (List [App "PVar" (List [App
    "Ident" (List [String "uniplate"])]),App "Nothing" (List []),App
    "UnGuardedRhs" (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "uniplateAll"])])])]),App "BDecls" (List [
    List []])])])]),App "InstDecl" (List [List [App "ClassA" (List [
    App "UnQual" (List [App "Ident" (List [String "Typeable"])]),List
    [App "TyVar" (List [App "Ident" (List [String "t"])])]]),App
    "ClassA" (List [App "UnQual" (List [App "Ident" (List [String
    "Typeable"])]),List [App "TyVar" (List [App "Ident" (List [String
    "a"])])]]),App "ClassA" (List [App "UnQual" (List [App "Ident" (
    List [String "Uniplate"])]),List [App "TyVar" (List [App "Ident" (
    List [String "t"])])]]),App "ClassA" (List [App "UnQual" (List [
    App "Ident" (List [String "PlateAll"])]),List [App "TyVar" (List [
    App "Ident" (List [String "a"])]),App "TyVar" (List [App "Ident" (
    List [String "t"])])]])],App "UnQual" (List [App "Ident" (List [
    String "PlateAll"])]),List [App "TyParen" (List [App "TyApp" (List
    [App "TyCon" (List [App "UnQual" (List [App "Ident" (List [
    DataName])])]),App "TyVar" (List [App "Ident" (List [String "a"])]
    )])]),App "TyVar" (List [App "Ident" (List [String "t"])])],List [
    App "InsDecl" (List [App "FunBind" (List [MapCtor (App "Match" (
    List [App "Ident" (List [String "plateAll"]),List [App "PParen" (
    List [App "PApp" (List [App "UnQual" (List [App "Ident" (List [
    CtorName])]),MapField (App "PVar" (List [App "Ident" (List [Concat
    (List [String "x",ShowInt FieldIndex])])]))])])],App "Nothing" (
    List []),App "UnGuardedRhs" (List [Fold (App "InfixApp" (List [
    Tail,App "QVarOp" (List [App "UnQual" (List [App "Symbol" (List [
    String "|+"])])]),Head])) (Concat (List [Reverse (MapField (App
    "Var" (List [App "UnQual" (List [App "Ident" (List [Concat (List [
    String "x",ShowInt FieldIndex])])])]))),List [App "App" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String "plate"
    ])])]),App "Con" (List [App "UnQual" (List [App "Ident" (List [
    CtorName])])])])]]))]),App "BDecls" (List [List []])]))])])]])]
-- GENERATED STOP
