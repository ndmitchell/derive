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
    List [Instance ["Typeable"] "Data" (List [App "InsDecl" (List [App
    "FunBind" (List [List [App "Match" (List [App "Ident" (List [
    String "gfoldl"]),List [App "PVar" (List [App "Ident" (List [
    String "k"])]),App "PVar" (List [App "Ident" (List [String "r"])])
    ,App "PVar" (List [App "Ident" (List [String "x"])])],App
    "Nothing" (List []),App "UnGuardedRhs" (List [App "App" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String "r"])])
    ]),App "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "x"])])])])]),App "Nothing" (List [])])]])]),App "InsDecl" (List [
    App "PatBind" (List [App "PVar" (List [App "Ident" (List [String
    "gunfold"])]),App "UnGuardedRhs" (List [App "App" (List [App "Var"
    (List [App "UnQual" (List [App "Ident" (List [String "error"])])])
    ,App "Lit" (List [App "String" (List [Concat (List [String
    "Data.gunfold not implemented on abstract data type: ",DataName])]
    )])])]),App "Nothing" (List [])])]),App "InsDecl" (List [App
    "PatBind" (List [App "PVar" (List [App "Ident" (List [String
    "toConstr"])]),App "UnGuardedRhs" (List [App "App" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String "error"
    ])])]),App "Lit" (List [App "String" (List [Concat (List [String
    "Data.gunfold not implemented on abstract data type: ",DataName])]
    )])])]),App "Nothing" (List [])])]),App "InsDecl" (List [App
    "PatBind" (List [App "PVar" (List [App "Ident" (List [String
    "dataTypeOf"])]),App "UnGuardedRhs" (List [App "App" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String "error"
    ])])]),App "Lit" (List [App "String" (List [Concat (List [String
    "Data.gunfold not implemented on abstract data type: ",DataName])]
    )])])]),App "Nothing" (List [])])])])]
-- GENERATED STOP

custom = customContext context

context :: FullDataDecl -> Context -> Context
context d _ = [ClassA (qname t) [tyVar x] | x <- dataDeclVars $ snd d, t <- ["Typeable","Data"]]
