module Data.Derive.Serialize where
{-
import "cereal" Data.Serialize

example :: Sample

instance Serialize alpha => Serialize (Sample alpha) where
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

instance Serialize a => Serialize (List a) where
    put x = case x of
        Nil -> putWord8 0
        Cons x1 x2 -> do putWord8 1; put x1; put x2

    get = do
        i <- getWord8
        case i of
            0 -> return Nil
            1 -> do x1 <- get; x2 <- get; return (Cons x1 x2)
            _ -> error "Corrupted binary data for List"

test :: Assoced

instance Serialize typ => Serialize (Assoced typ) where
    put (Assoced x1 x2) = do put x1; put x2
    get = do x1 <- get; x2 <- get; return (Assoced x1 x2)


-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeSerialize :: Derivation
makeSerialize = derivationDSL "Serialize" dslSerialize

dslSerialize =
    List [Instance ["Serialize"] "Serialize" (App "Just" (List [List [
    App "InsDecl" (List [App "()" (List []),App "FunBind" (List [App
    "()" (List []),List [App "Match" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "put"]),List [App "PVar"
    (List [App "()" (List []),App "Ident" (List [App "()" (List []),
    String "x"])])],App "UnGuardedRhs" (List [App "()" (List []),App
    "Case" (List [App "()" (List []),App "Var" (List [App "()" (List [
    ]),App "UnQual" (List [App "()" (List []),App "Ident" (List [App
    "()" (List []),String "x"])])]),MapCtor (App "Alt" (List [App "()"
    (List []),App "PApp" (List [App "()" (List []),App "UnQual" (List
    [App "()" (List []),App "Ident" (List [App "()" (List []),CtorName
    ])]),MapField (App "PVar" (List [App "()" (List []),App "Ident" (
    List [App "()" (List []),Concat (List [String "x",ShowInt
    FieldIndex])])]))]),App "UnGuardedRhs" (List [App "()" (List []),
    App "Do" (List [App "()" (List []),Concat (List [List [App
    "Qualifier" (List [App "()" (List []),App "App" (List [App "()" (
    List []),App "Var" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Ident" (List [App "()" (List []),String
    "putTag"])])]),App "Lit" (List [App "()" (List []),App "Int" (List
    [App "()" (List []),CtorIndex,ShowInt CtorIndex])])])])],MapField
    (App "Qualifier" (List [App "()" (List []),App "App" (List [App
    "()" (List []),App "Var" (List [App "()" (List []),App "UnQual" (
    List [App "()" (List []),App "Ident" (List [App "()" (List []),
    String "put"])])]),App "Var" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),Concat (List [String "x",ShowInt FieldIndex])])])])])]))]
    )])]),App "Nothing" (List [])]))])]),App "Just" (List [App
    "BDecls" (List [App "()" (List []),List [App "PatBind" (List [App
    "()" (List []),App "PVar" (List [App "()" (List []),App "Ident" (
    List [App "()" (List []),String "useTag"])]),App "UnGuardedRhs" (
    List [App "()" (List []),App "InfixApp" (List [App "()" (List []),
    App "App" (List [App "()" (List []),App "Var" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "length"])])]),App "List" (List [App
    "()" (List []),MapCtor (App "RecConstr" (List [App "()" (List []),
    App "UnQual" (List [App "()" (List []),App "Ident" (List [App "()"
    (List []),CtorName])]),List []]))])]),App "QVarOp" (List [App "()"
    (List []),App "UnQual" (List [App "()" (List []),App "Symbol" (
    List [App "()" (List []),String ">"])])]),App "Lit" (List [App
    "()" (List []),App "Int" (List [App "()" (List []),Int 1,ShowInt (
    Int 1)])])])]),App "Nothing" (List [])]),App "FunBind" (List [App
    "()" (List []),List [App "Match" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "putTag"]),List [App
    "PVar" (List [App "()" (List []),App "Ident" (List [App "()" (List
    []),String "x"])])],App "UnGuardedRhs" (List [App "()" (List []),
    Application (List [App "Var" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "when"])])]),App "Var" (List [App "()" (List []),
    App "UnQual" (List [App "()" (List []),App "Ident" (List [App "()"
    (List []),String "useTag"])])]),App "Paren" (List [App "()" (List
    []),App "App" (List [App "()" (List []),App "Var" (List [App "()"
    (List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),Concat (List [String "putWord",ShowInt (Int 8)
    ])])])]),App "Var" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Ident" (List [App "()" (List []),String
    "x"])])])])])])]),App "Nothing" (List [])])]])]])])])]])]),App
    "InsDecl" (List [App "()" (List []),App "PatBind" (List [App "()"
    (List []),App "PVar" (List [App "()" (List []),App "Ident" (List [
    App "()" (List []),String "get"])]),App "UnGuardedRhs" (List [App
    "()" (List []),App "Do" (List [App "()" (List []),List [App
    "Generator" (List [App "()" (List []),App "PVar" (List [App "()" (
    List []),App "Ident" (List [App "()" (List []),String "i"])]),App
    "Var" (List [App "()" (List []),App "UnQual" (List [App "()" (List
    []),App "Ident" (List [App "()" (List []),String "getTag"])])])]),
    App "Qualifier" (List [App "()" (List []),App "Case" (List [App
    "()" (List []),App "Var" (List [App "()" (List []),App "UnQual" (
    List [App "()" (List []),App "Ident" (List [App "()" (List []),
    String "i"])])]),Concat (List [MapCtor (App "Alt" (List [App "()"
    (List []),App "PLit" (List [App "()" (List []),App "Signless" (
    List [App "()" (List [])]),App "Int" (List [App "()" (List []),
    CtorIndex,ShowInt CtorIndex])]),App "UnGuardedRhs" (List [App "()"
    (List []),App "Do" (List [App "()" (List []),Concat (List [
    MapField (App "Generator" (List [App "()" (List []),App "PVar" (
    List [App "()" (List []),App "Ident" (List [App "()" (List []),
    Concat (List [String "x",ShowInt FieldIndex])])]),App "Var" (List
    [App "()" (List []),App "UnQual" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "get"])])])])),List [App
    "Qualifier" (List [App "()" (List []),App "App" (List [App "()" (
    List []),App "Var" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Ident" (List [App "()" (List []),String
    "return"])])]),App "Paren" (List [App "()" (List []),Application (
    Concat (List [List [App "Con" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),CtorName])])])],MapField (App "Var" (List [App "()" (List
    []),App "UnQual" (List [App "()" (List []),App "Ident" (List [App
    "()" (List []),Concat (List [String "x",ShowInt FieldIndex])])])])
    )]))])])])]])])]),App "Nothing" (List [])])),List [App "Alt" (List
    [App "()" (List []),App "PWildCard" (List [App "()" (List [])]),
    App "UnGuardedRhs" (List [App "()" (List []),App "App" (List [App
    "()" (List []),App "Var" (List [App "()" (List []),App "UnQual" (
    List [App "()" (List []),App "Ident" (List [App "()" (List []),
    String "error"])])]),App "Lit" (List [App "()" (List []),App
    "String" (List [App "()" (List []),Concat (List [String
    "Corrupted binary data for ",DataName]),Concat (List [String
    "Corrupted binary data for ",DataName])])])])]),App "Nothing" (
    List [])])]])])])]])]),App "Just" (List [App "BDecls" (List [App
    "()" (List []),List [App "PatBind" (List [App "()" (List []),App
    "PVar" (List [App "()" (List []),App "Ident" (List [App "()" (List
    []),String "useTag"])]),App "UnGuardedRhs" (List [App "()" (List [
    ]),App "InfixApp" (List [App "()" (List []),App "App" (List [App
    "()" (List []),App "Var" (List [App "()" (List []),App "UnQual" (
    List [App "()" (List []),App "Ident" (List [App "()" (List []),
    String "length"])])]),App "List" (List [App "()" (List []),MapCtor
    (App "RecConstr" (List [App "()" (List []),App "UnQual" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),CtorName])]),
    List []]))])]),App "QVarOp" (List [App "()" (List []),App "UnQual"
    (List [App "()" (List []),App "Symbol" (List [App "()" (List []),
    String ">"])])]),App "Lit" (List [App "()" (List []),App "Int" (
    List [App "()" (List []),Int 1,ShowInt (Int 1)])])])]),App
    "Nothing" (List [])]),App "PatBind" (List [App "()" (List []),App
    "PVar" (List [App "()" (List []),App "Ident" (List [App "()" (List
    []),String "getTag"])]),App "UnGuardedRhs" (List [App "()" (List [
    ]),App "If" (List [App "()" (List []),App "Var" (List [App "()" (
    List []),App "UnQual" (List [App "()" (List []),App "Ident" (List
    [App "()" (List []),String "useTag"])])]),App "Var" (List [App
    "()" (List []),App "UnQual" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),Concat (List [String "getWord",ShowInt (
    Int 8)])])])]),App "App" (List [App "()" (List []),App "Var" (List
    [App "()" (List []),App "UnQual" (List [App "()" (List []),App
    "Ident" (List [App "()" (List []),String "return"])])]),App "Lit"
    (List [App "()" (List []),App "Int" (List [App "()" (List []),Int
    0,ShowInt (Int 0)])])])])]),App "Nothing" (List [])])]])])])])]]))
    ]
-- GENERATED STOP
