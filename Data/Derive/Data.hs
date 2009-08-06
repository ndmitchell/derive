module Data.Derive.Data(makeData) where
{-
import Data.Data

example :: Custom

instance (Data a, Typeable a) => Data (Sample a) where
    gfoldl k r (First) = r First
    gfoldl k r (Second x1 x2) = r Second `k` x1 `k` x2
    gfoldl k r (Third x1) = r Third `k` x1

    gunfold k z c = case constrIndex c - 1 of
        0 -> z First
        1 -> const k 1 $ const k 2 $ z Second
        2 -> const k 1 $ z Third
        i -> error $ ("Data.gunfold for Sample" ++ ", unknown index: ") ++ show i

    toConstr x@First{}  = indexConstr (dataTypeOf x) (0+1)
    toConstr x@Second{} = indexConstr (dataTypeOf x) (1+1)
    toConstr x@Third{}  = indexConstr (dataTypeOf x) (2+1)

    dataTypeOf _ = ty
        where ty = mkDataType $(dataName)
                   [mkConstr ty "First"  $(ctorFields 0) $(ctorFixity 0)
                   ,mkConstr ty "Second" $(ctorFields 1) $(ctorFixity 1)
                   ,mkConstr ty "Third"  $(ctorFields 2) $(ctorFixity 2)]

test :: Computer

instance Data Computer where
    gfoldl k r (Laptop x1 x2) = r Laptop `k` x1 `k` x2
    gfoldl k r (Desktop x1) = r Desktop `k` x1
    gunfold k z c = case constrIndex c - 1 of
        0 -> k $ k $ z Laptop
        1 -> k $ z Desktop
        i -> error $ "Data.gunfold for Computer, unknown index: " ++ show i
    toConstr x@Laptop{} = indexConstr (dataTypeOf x) 1
    toConstr x@Desktop{} = indexConstr (dataTypeOf x) 2
    dataTypeOf _ = ty
      where ty = mkDataType "Example.Computer"
                 [mkConstr ty "Laptop" ["weight", "speed"] Prefix
                 ,mkConstr ty "Desktop" ["speed"] Prefix]

-}

import Data.Derive.DSL.HSE
import qualified Language.Haskell as H

-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeData :: Derivation
makeData = derivationCustomDSL "Data" custom $
    List [Instance ["Data","Typeable"] "Data" (List [App "InsDecl" (
    List [App "FunBind" (List [MapCtor (App "Match" (List [App "Ident"
    (List [String "gfoldl"]),List [App "PVar" (List [App "Ident" (List
    [String "k"])]),App "PVar" (List [App "Ident" (List [String "r"])]
    ),App "PParen" (List [App "PApp" (List [App "UnQual" (List [App
    "Ident" (List [CtorName])]),MapField (App "PVar" (List [App
    "Ident" (List [Concat (List [String "x",ShowInt FieldIndex])])]))]
    )])],App "Nothing" (List []),App "UnGuardedRhs" (List [Fold (App
    "InfixApp" (List [Tail,App "QVarOp" (List [App "UnQual" (List [App
    "Ident" (List [String "k"])])]),Head])) (Concat (List [Reverse (
    MapField (App "Var" (List [App "UnQual" (List [App "Ident" (List [
    Concat (List [String "x",ShowInt FieldIndex])])])]))),List [App
    "App" (List [App "Var" (List [App "UnQual" (List [App "Ident" (
    List [String "r"])])]),App "Con" (List [App "UnQual" (List [App
    "Ident" (List [CtorName])])])])]]))]),App "BDecls" (List [List []]
    )]))])]),App "InsDecl" (List [App "FunBind" (List [List [App
    "Match" (List [App "Ident" (List [String "gunfold"]),List [App
    "PVar" (List [App "Ident" (List [String "k"])]),App "PVar" (List [
    App "Ident" (List [String "z"])]),App "PVar" (List [App "Ident" (
    List [String "c"])])],App "Nothing" (List []),App "UnGuardedRhs" (
    List [App "Case" (List [App "InfixApp" (List [App "App" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "constrIndex"])])]),App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "c"])])])]),App "QVarOp" (List [App "UnQual"
    (List [App "Symbol" (List [String "-"])])]),App "Lit" (List [App
    "Int" (List [Int 1])])]),Concat (List [MapCtor (App "Alt" (List [
    App "PLit" (List [App "Int" (List [CtorIndex])]),App
    "UnGuardedAlt" (List [Fold (App "InfixApp" (List [Head,App
    "QVarOp" (List [App "UnQual" (List [App "Symbol" (List [String "$"
    ])])]),Tail])) (Concat (List [MapField (Application (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String "const"
    ])])]),App "Var" (List [App "UnQual" (List [App "Ident" (List [
    String "k"])])]),App "Lit" (List [App "Int" (List [FieldIndex])])]
    )),List [App "App" (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "z"])])]),App "Con" (List [App "UnQual" (
    List [App "Ident" (List [CtorName])])])])]]))]),App "BDecls" (List
    [List []])])),List [App "Alt" (List [App "PVar" (List [App "Ident"
    (List [String "i"])]),App "UnGuardedAlt" (List [App "InfixApp" (
    List [App "Var" (List [App "UnQual" (List [App "Ident" (List [
    String "error"])])]),App "QVarOp" (List [App "UnQual" (List [App
    "Symbol" (List [String "$"])])]),App "InfixApp" (List [App "Paren"
    (List [App "InfixApp" (List [App "Lit" (List [App "String" (List [
    Concat (List [String "Data.gunfold for ",DataName])])]),App
    "QVarOp" (List [App "UnQual" (List [App "Symbol" (List [String
    "++"])])]),App "Lit" (List [App "String" (List [String
    ", unknown index: "])])])]),App "QVarOp" (List [App "UnQual" (List
    [App "Symbol" (List [String "++"])])]),App "App" (List [App "Var"
    (List [App "UnQual" (List [App "Ident" (List [String "show"])])]),
    App "Var" (List [App "UnQual" (List [App "Ident" (List [String "i"
    ])])])])])])]),App "BDecls" (List [List []])])]])])]),App "BDecls"
    (List [List []])])]])]),App "InsDecl" (List [App "FunBind" (List [
    MapCtor (App "Match" (List [App "Ident" (List [String "toConstr"])
    ,List [App "PAsPat" (List [App "Ident" (List [String "x"]),App
    "PRec" (List [App "UnQual" (List [App "Ident" (List [CtorName])]),
    List []])])],App "Nothing" (List []),App "UnGuardedRhs" (List [
    Application (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "indexConstr"])])]),App "Paren" (List [App
    "App" (List [App "Var" (List [App "UnQual" (List [App "Ident" (
    List [String "dataTypeOf"])])]),App "Var" (List [App "UnQual" (
    List [App "Ident" (List [String "x"])])])])]),App "Paren" (List [
    App "InfixApp" (List [App "Lit" (List [App "Int" (List [CtorIndex]
    )]),App "QVarOp" (List [App "UnQual" (List [App "Symbol" (List [
    String "+"])])]),App "Lit" (List [App "Int" (List [Int 1])])])])])
    ]),App "BDecls" (List [List []])]))])]),App "InsDecl" (List [App
    "FunBind" (List [List [App "Match" (List [App "Ident" (List [
    String "dataTypeOf"]),List [App "PWildCard" (List [])],App
    "Nothing" (List []),App "UnGuardedRhs" (List [App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "ty"])])])]),App
    "BDecls" (List [List [App "PatBind" (List [App "PVar" (List [App
    "Ident" (List [String "ty"])]),App "Nothing" (List []),App
    "UnGuardedRhs" (List [Application (List [App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "mkDataType"])])]),App
    "SpliceExp" (List [App "ParenSplice" (List [App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "dataName"])])])])]),App
    "List" (List [MapCtor (Application (List [App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "mkConstr"])])]),App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String "ty"])]
    )]),App "Lit" (List [App "String" (List [CtorName])]),App
    "SpliceExp" (List [App "ParenSplice" (List [App "App" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "ctorFields"])])]),App "Lit" (List [App "Int" (List [CtorIndex])])
    ])])]),App "SpliceExp" (List [App "ParenSplice" (List [App "App" (
    List [App "Var" (List [App "UnQual" (List [App "Ident" (List [
    String "ctorFixity"])])]),App "Lit" (List [App "Int" (List [
    CtorIndex])])])])])]))])])]),App "BDecls" (List [List []])])]])])]
    ])])])]
-- GENERATED STOP


custom d = customContext context d . customSplice splice d

splice :: FullDataDecl -> Exp -> Exp
splice d x | x ~= "dataName" = H.Lit $ H.String $ prettyPrint (fst d) ++ "." ++ dataDeclName (snd d)
splice d (H.App x (H.Lit (H.Int y)))
    | x ~= "ctorFields" = H.List $ [H.Lit $ H.String a | (a,_) <- ctorDeclFields ctor, a /= ""]
    | x ~= "ctorFixity" = Con (UnQual (Ident "Prefix"))
    where ctor = dataDeclCtors (snd d) !! fromInteger y

context :: FullDataDecl -> Context
context d = [ClassA (qname t) [tyVar x] | x <- dataDeclVars $ snd d, t <- ["Typeable","Data"]]
