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
    List [Instance ["Data","Typeable"] "Data" (App "Just" (List [List
    [App "InsDecl" (List [App "()" (List []),App "FunBind" (List [App
    "()" (List []),MapCtor (App "Match" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "gfoldl"]),List [App
    "PVar" (List [App "()" (List []),App "Ident" (List [App "()" (List
    []),String "k"])]),App "PVar" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "r"])]),App "PParen" (
    List [App "()" (List []),App "PApp" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),CtorName])]),MapField (App "PVar" (List [App "()" (List [
    ]),App "Ident" (List [App "()" (List []),Concat (List [String "x",
    ShowInt FieldIndex])])]))])])],App "UnGuardedRhs" (List [App "()"
    (List []),Fold (App "InfixApp" (List [App "()" (List []),Tail,App
    "QVarOp" (List [App "()" (List []),App "UnQual" (List [App "()" (
    List []),App "Ident" (List [App "()" (List []),String "k"])])]),
    Head])) (Concat (List [Reverse (MapField (App "Var" (List [App
    "()" (List []),App "UnQual" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),Concat (List [String "x",ShowInt
    FieldIndex])])])]))),List [App "App" (List [App "()" (List []),App
    "Var" (List [App "()" (List []),App "UnQual" (List [App "()" (List
    []),App "Ident" (List [App "()" (List []),String "r"])])]),App
    "Con" (List [App "()" (List []),App "UnQual" (List [App "()" (List
    []),App "Ident" (List [App "()" (List []),CtorName])])])])]]))]),
    App "Nothing" (List [])]))])]),App "InsDecl" (List [App "()" (List
    []),App "FunBind" (List [App "()" (List []),List [App "Match" (
    List [App "()" (List []),App "Ident" (List [App "()" (List []),
    String "gunfold"]),List [App "PVar" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "k"])]),App "PVar" (List
    [App "()" (List []),App "Ident" (List [App "()" (List []),String
    "z"])]),App "PVar" (List [App "()" (List []),App "Ident" (List [
    App "()" (List []),String "c"])])],App "UnGuardedRhs" (List [App
    "()" (List []),App "Case" (List [App "()" (List []),App "InfixApp"
    (List [App "()" (List []),App "App" (List [App "()" (List []),App
    "Var" (List [App "()" (List []),App "UnQual" (List [App "()" (List
    []),App "Ident" (List [App "()" (List []),String "constrIndex"])])
    ]),App "Var" (List [App "()" (List []),App "UnQual" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),String "c"])]
    )])]),App "QVarOp" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Symbol" (List [App "()" (List []),String
    "-"])])]),App "Lit" (List [App "()" (List []),App "Int" (List [App
    "()" (List []),Int 1,ShowInt (Int 1)])])]),Concat (List [MapCtor (
    App "Alt" (List [App "()" (List []),App "PLit" (List [App "()" (
    List []),App "Signless" (List [App "()" (List [])]),App "Int" (
    List [App "()" (List []),CtorIndex,ShowInt CtorIndex])]),App
    "UnGuardedRhs" (List [App "()" (List []),Fold (App "InfixApp" (
    List [App "()" (List []),Head,App "QVarOp" (List [App "()" (List [
    ]),App "UnQual" (List [App "()" (List []),App "Symbol" (List [App
    "()" (List []),String "$"])])]),Tail])) (Concat (List [MapField (
    Application (List [App "Var" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "const"])])]),App "Var" (List [App "()" (List []),
    App "UnQual" (List [App "()" (List []),App "Ident" (List [App "()"
    (List []),String "k"])])]),App "Lit" (List [App "()" (List []),App
    "Int" (List [App "()" (List []),FieldIndex,ShowInt FieldIndex])])]
    )),List [App "App" (List [App "()" (List []),App "Var" (List [App
    "()" (List []),App "UnQual" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),String "z"])])]),App "Con" (List [App
    "()" (List []),App "UnQual" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),CtorName])])])])]]))]),App "Nothing" (
    List [])])),List [App "Alt" (List [App "()" (List []),App "PVar" (
    List [App "()" (List []),App "Ident" (List [App "()" (List []),
    String "i"])]),App "UnGuardedRhs" (List [App "()" (List []),App
    "InfixApp" (List [App "()" (List []),App "Var" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "error"])])]),App "QVarOp" (List [App
    "()" (List []),App "UnQual" (List [App "()" (List []),App "Symbol"
    (List [App "()" (List []),String "$"])])]),App "InfixApp" (List [
    App "()" (List []),App "Paren" (List [App "()" (List []),App
    "InfixApp" (List [App "()" (List []),App "Lit" (List [App "()" (
    List []),App "String" (List [App "()" (List []),Concat (List [
    String "Data.gunfold for ",DataName]),Concat (List [String
    "Data.gunfold for ",DataName])])]),App "QVarOp" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Symbol" (List
    [App "()" (List []),String "++"])])]),App "Lit" (List [App "()" (
    List []),App "String" (List [App "()" (List []),String
    ", unknown index: ",String ", unknown index: "])])])]),App
    "QVarOp" (List [App "()" (List []),App "UnQual" (List [App "()" (
    List []),App "Symbol" (List [App "()" (List []),String "++"])])]),
    App "App" (List [App "()" (List []),App "Var" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "show"])])]),App "Var" (List [App "()"
    (List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "i"])])])])])])]),App "Nothing" (List [
    ])])]])])]),App "Nothing" (List [])])]])]),App "InsDecl" (List [
    App "()" (List []),App "FunBind" (List [App "()" (List []),MapCtor
    (App "Match" (List [App "()" (List []),App "Ident" (List [App "()"
    (List []),String "toConstr"]),List [App "PAsPat" (List [App "()" (
    List []),App "Ident" (List [App "()" (List []),String "x"]),App
    "PRec" (List [App "()" (List []),App "UnQual" (List [App "()" (
    List []),App "Ident" (List [App "()" (List []),CtorName])]),List [
    ]])])],App "UnGuardedRhs" (List [App "()" (List []),Application (
    List [App "Var" (List [App "()" (List []),App "UnQual" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),String
    "indexConstr"])])]),App "Paren" (List [App "()" (List []),App
    "App" (List [App "()" (List []),App "Var" (List [App "()" (List []
    ),App "UnQual" (List [App "()" (List []),App "Ident" (List [App
    "()" (List []),String "dataTypeOf"])])]),App "Var" (List [App "()"
    (List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "x"])])])])]),App "Paren" (List [App
    "()" (List []),App "InfixApp" (List [App "()" (List []),App "Lit"
    (List [App "()" (List []),App "Int" (List [App "()" (List []),
    CtorIndex,ShowInt CtorIndex])]),App "QVarOp" (List [App "()" (List
    []),App "UnQual" (List [App "()" (List []),App "Symbol" (List [App
    "()" (List []),String "+"])])]),App "Lit" (List [App "()" (List []
    ),App "Int" (List [App "()" (List []),Int 1,ShowInt (Int 1)])])])]
    )])]),App "Nothing" (List [])]))])])]]))]
-- GENERATED STOP


custom d = customContext context d . customSplice splice d

splice :: FullDataDecl -> Exp () -> Exp ()
splice d x | x ~= "dataName" = H.Lit () $ H.String () s s
    where s = prettyPrint (fst d) ++ "." ++ dataDeclName (snd d)
splice d (H.App _ x (H.Lit _ (H.Int _ y _)))
    | x ~= "ctorFields" = H.List () [H.Lit () $ H.String () a a | (a,_) <- ctorDeclFields ctor, a /= ""]
    | x ~= "ctorFixity" = Con () $ UnQual () $ Ident () "Prefix"
    where ctor = dataDeclCtors (snd d) !! fromInteger y

context :: FullDataDecl -> Context () -> Context ()
context d _ = CxTuple () [ClassA () (qname t) [tyVar x] | x <- dataDeclVarsStar $ snd d, t <- ["Typeable","Data"]]
