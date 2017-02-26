{-|
    For deriving Data on abstract data types.
-}
module Data.Derive.DataAbstract(makeDataAbstract) where
{-
import Data.Data(Data(..))

example :: Custom

instance Typeable a => Data (Sample a) where
    gfoldl k r x = r x
    gunfold = error "Data.gunfold not implemented on abstract data type: Sample"
    toConstr = error "Data.gunfold not implemented on abstract data type: Sample"
    dataTypeOf = error "Data.gunfold not implemented on abstract data type: Sample"

-}

import Data.Derive.DSL.HSE

-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeDataAbstract :: Derivation
makeDataAbstract = derivationCustomDSL "DataAbstract" custom $
    List [Instance ["Typeable"] "Data" (App "Just" (List [List [App
    "InsDecl" (List [App "()" (List []),App "FunBind" (List [App "()"
    (List []),List [App "Match" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),String "gfoldl"]),List [App "PVar" (List
    [App "()" (List []),App "Ident" (List [App "()" (List []),String
    "k"])]),App "PVar" (List [App "()" (List []),App "Ident" (List [
    App "()" (List []),String "r"])]),App "PVar" (List [App "()" (List
    []),App "Ident" (List [App "()" (List []),String "x"])])],App
    "UnGuardedRhs" (List [App "()" (List []),App "App" (List [App "()"
    (List []),App "Var" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Ident" (List [App "()" (List []),String
    "r"])])]),App "Var" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Ident" (List [App "()" (List []),String
    "x"])])])])]),App "Nothing" (List [])])]])]),App "InsDecl" (List [
    App "()" (List []),App "PatBind" (List [App "()" (List []),App
    "PVar" (List [App "()" (List []),App "Ident" (List [App "()" (List
    []),String "gunfold"])]),App "UnGuardedRhs" (List [App "()" (List
    []),App "App" (List [App "()" (List []),App "Var" (List [App "()"
    (List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "error"])])]),App "Lit" (List [App "()"
    (List []),App "String" (List [App "()" (List []),Concat (List [
    String "Data.gunfold not implemented on abstract data type: ",
    DataName]),Concat (List [String
    "Data.gunfold not implemented on abstract data type: ",DataName])]
    )])])]),App "Nothing" (List [])])]),App "InsDecl" (List [App "()"
    (List []),App "PatBind" (List [App "()" (List []),App "PVar" (List
    [App "()" (List []),App "Ident" (List [App "()" (List []),String
    "toConstr"])]),App "UnGuardedRhs" (List [App "()" (List []),App
    "App" (List [App "()" (List []),App "Var" (List [App "()" (List []
    ),App "UnQual" (List [App "()" (List []),App "Ident" (List [App
    "()" (List []),String "error"])])]),App "Lit" (List [App "()" (
    List []),App "String" (List [App "()" (List []),Concat (List [
    String "Data.gunfold not implemented on abstract data type: ",
    DataName]),Concat (List [String
    "Data.gunfold not implemented on abstract data type: ",DataName])]
    )])])]),App "Nothing" (List [])])]),App "InsDecl" (List [App "()"
    (List []),App "PatBind" (List [App "()" (List []),App "PVar" (List
    [App "()" (List []),App "Ident" (List [App "()" (List []),String
    "dataTypeOf"])]),App "UnGuardedRhs" (List [App "()" (List []),App
    "App" (List [App "()" (List []),App "Var" (List [App "()" (List []
    ),App "UnQual" (List [App "()" (List []),App "Ident" (List [App
    "()" (List []),String "error"])])]),App "Lit" (List [App "()" (
    List []),App "String" (List [App "()" (List []),Concat (List [
    String "Data.gunfold not implemented on abstract data type: ",
    DataName]),Concat (List [String
    "Data.gunfold not implemented on abstract data type: ",DataName])]
    )])])]),App "Nothing" (List [])])])]]))]
-- GENERATED STOP

custom = customContext context

context :: FullDataDecl -> Context () -> Context ()
context d _ = CxTuple () [ClassA () (qname t) [tyVar x] | x <- dataDeclVars $ snd d, t <- ["Typeable","Data"]]
