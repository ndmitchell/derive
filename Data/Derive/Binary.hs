module Data.Derive.Binary where
{-
import "binary" Data.Binary

example :: Sample

instance Binary alpha => Binary (Sample alpha) where
    put x = case x of
        First          -> do putTag 0
        Second  x1 x2  -> do putTag 1 ; put x1 ; put x2
        Third   x1     -> do putTag 2 ; put x1
        where
            useTag = length [First{}, Second{}, Third{}] > 1
            putTag x = when useTag (putWord8 x)

    get = do
        i <- getTag
        case i of
            0 -> do return (First)
            1 -> do x1 <- get ; x2 <- get ; return (Second x1 x2)
            2 -> do x1 <- get ; return (Third x1)
            _ -> error "Corrupted binary data for Sample"
        where
            useTag = length [First{}, Second{}, Third{}] > 1
            getTag = if useTag then getWord8 else return 0


test :: List

instance Binary a => Binary (List a) where
    put x = case x of
        Nil -> putWord8 0
        Cons x1 x2 -> do putWord8 1; put x1; put x2

    get = do
        i <- getWord8
        case i of
            0 -> return Nil
            1 -> do x1 <- get; x2 <- get; return (Cons x1 x2)
            _ -> error "Corrupted binary data for List"

test :: Assoc

instance Binary a => Binary (Assoc a) where
    put (Assoc x1 x2) = do put x1; put x2
    get = do x1 <- get; x2 <- get; return (Assoc x1 x2)


-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeBinary :: Derivation
makeBinary = derivationDSL "Binary" dslBinary

dslBinary =
    List [Instance ["Binary"] "Binary" (List [App "InsDecl" (List [App
    "FunBind" (List [List [App "Match" (List [App "Ident" (List [
    String "put"]),List [App "PVar" (List [App "Ident" (List [String
    "x"])])],App "Nothing" (List []),App "UnGuardedRhs" (List [App
    "Case" (List [App "Var" (List [App "UnQual" (List [App "Ident" (
    List [String "x"])])]),MapCtor (App "Alt" (List [App "PApp" (List
    [App "UnQual" (List [App "Ident" (List [CtorName])]),MapField (App
    "PVar" (List [App "Ident" (List [Concat (List [String "x",ShowInt
    FieldIndex])])]))]),App "UnGuardedAlt" (List [App "Do" (List [
    Concat (List [List [App "Qualifier" (List [App "App" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "putTag"])])]),App "Lit" (List [App "Int" (List [CtorIndex])])])])
    ],MapField (App "Qualifier" (List [App "App" (List [App "Var" (
    List [App "UnQual" (List [App "Ident" (List [String "put"])])]),
    App "Var" (List [App "UnQual" (List [App "Ident" (List [Concat (
    List [String "x",ShowInt FieldIndex])])])])])]))])])]),App
    "BDecls" (List [List []])]))])]),App "BDecls" (List [List [App
    "PatBind" (List [App "PVar" (List [App "Ident" (List [String
    "useTag"])]),App "Nothing" (List []),App "UnGuardedRhs" (List [App
    "InfixApp" (List [App "App" (List [App "Var" (List [App "UnQual" (
    List [App "Ident" (List [String "length"])])]),App "List" (List [
    MapCtor (App "RecConstr" (List [App "UnQual" (List [App "Ident" (
    List [CtorName])]),List []]))])]),App "QVarOp" (List [App "UnQual"
    (List [App "Symbol" (List [String ">"])])]),App "Lit" (List [App
    "Int" (List [Int 1])])])]),App "BDecls" (List [List []])]),App
    "FunBind" (List [List [App "Match" (List [App "Ident" (List [
    String "putTag"]),List [App "PVar" (List [App "Ident" (List [
    String "x"])])],App "Nothing" (List []),App "UnGuardedRhs" (List [
    Application (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "when"])])]),App "Var" (List [App "UnQual" (
    List [App "Ident" (List [String "useTag"])])]),App "Paren" (List [
    App "App" (List [App "Var" (List [App "UnQual" (List [App "Ident"
    (List [Concat (List [String "putWord",ShowInt (Int 8)])])])]),App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String "x"])])
    ])])])])]),App "BDecls" (List [List []])])]])]])])]])]),App
    "InsDecl" (List [App "PatBind" (List [App "PVar" (List [App
    "Ident" (List [String "get"])]),App "Nothing" (List []),App
    "UnGuardedRhs" (List [App "Do" (List [List [App "Generator" (List
    [App "PVar" (List [App "Ident" (List [String "i"])]),App "Var" (
    List [App "UnQual" (List [App "Ident" (List [String "getTag"])])])
    ]),App "Qualifier" (List [App "Case" (List [App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "i"])])]),Concat (List [
    MapCtor (App "Alt" (List [App "PLit" (List [App "Int" (List [
    CtorIndex])]),App "UnGuardedAlt" (List [App "Do" (List [Concat (
    List [MapField (App "Generator" (List [App "PVar" (List [App
    "Ident" (List [Concat (List [String "x",ShowInt FieldIndex])])]),
    App "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "get"])])])])),List [App "Qualifier" (List [App "App" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "return"])])]),App "Paren" (List [Application (Concat (List [List
    [App "Con" (List [App "UnQual" (List [App "Ident" (List [CtorName]
    )])])],MapField (App "Var" (List [App "UnQual" (List [App "Ident"
    (List [Concat (List [String "x",ShowInt FieldIndex])])])]))]))])])
    ])]])])]),App "BDecls" (List [List []])])),List [App "Alt" (List [
    App "PWildCard" (List []),App "UnGuardedAlt" (List [App "App" (
    List [App "Var" (List [App "UnQual" (List [App "Ident" (List [
    String "error"])])]),App "Lit" (List [App "String" (List [Concat (
    List [String "Corrupted binary data for ",DataName])])])])]),App
    "BDecls" (List [List []])])]])])])]])]),App "BDecls" (List [List [
    App "PatBind" (List [App "PVar" (List [App "Ident" (List [String
    "useTag"])]),App "Nothing" (List []),App "UnGuardedRhs" (List [App
    "InfixApp" (List [App "App" (List [App "Var" (List [App "UnQual" (
    List [App "Ident" (List [String "length"])])]),App "List" (List [
    MapCtor (App "RecConstr" (List [App "UnQual" (List [App "Ident" (
    List [CtorName])]),List []]))])]),App "QVarOp" (List [App "UnQual"
    (List [App "Symbol" (List [String ">"])])]),App "Lit" (List [App
    "Int" (List [Int 1])])])]),App "BDecls" (List [List []])]),App
    "PatBind" (List [App "PVar" (List [App "Ident" (List [String
    "getTag"])]),App "Nothing" (List []),App "UnGuardedRhs" (List [App
    "If" (List [App "Var" (List [App "UnQual" (List [App "Ident" (List
    [String "useTag"])])]),App "Var" (List [App "UnQual" (List [App
    "Ident" (List [Concat (List [String "getWord",ShowInt (Int 8)])])]
    )]),App "App" (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "return"])])]),App "Lit" (List [App "Int" (
    List [Int 0])])])])]),App "BDecls" (List [List []])])]])])])])]
-- GENERATED STOP
