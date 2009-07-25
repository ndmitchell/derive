{-|
    Derives @Read@.  This is as defined by the Haskell report, except
    there is no support for infix constructors.  If you attempt to
    derive @Read@ for a data type with infix constructors, the
    constructors are handled as if they were prefix constructors, using
    the @(@/consym/@)@ syntax.
-}
module Data.Derive.Read(makeRead) where

{-
example :: Custom
instance Read a => Read (Sample a) where
    readsPrec p0 r = 
        readParen $(bracket 0) (\r0 -> $(comp 0)) r ++
        readParen $(bracket 1) (\r0 -> $(comp 1)) r ++
        readParen $(bracket 2) (\r0 -> $(comp 2)) r ++
        []

test :: Sample
instance Read a => Read (Sample a) where
    readsPrec p0 r =
        readParen (p0 > 10) (\r0 ->
            [ (First, r1)
            | ("First", r1) <- lex r0]) r
        ++
        readParen (p0 > 10) (\r0 ->
            [ (Second x1 x2, r3)
            | ("Second", r1) <- lex r0
            , (x1, r2) <- readsPrec 11 r1
            , (x2, r3) <- readsPrec 11 r2]) r
        ++
        readParen (p0 > 10) (\r0 ->
            [ (Third x1, r2)
            | ("Third", r1) <- lex r0
            , (x1, r2) <- readsPrec 11 r1]) r

test :: Computer
instance Read Computer where
    readsPrec p0 r =
        readParen False (\r0 ->
            [ (Laptop x1 x2, r10)
            | ("Laptop", r1) <- lex r0
            , ("{", r2) <- lex r1
            , ("weight", r3) <- lex r2
            , ("=", r4) <- lex r3
            , (x1, r5) <- readsPrec 0 r4
            , (",", r6) <- lex r5
            , ("speed", r7) <- lex r6
            , ("=", r8) <- lex r7
            , (x2, r9) <- readsPrec 0 r8
            , ("}", r10) <- lex r9]) r
        ++
        readParen False (\r0 ->
            [ (Desktop x1, r6)
            | ("Desktop", r1) <- lex r0
            , ("{", r2) <- lex r1
            , ("speed", r3) <- lex r2
            , ("=", r4) <- lex r3
            , (x1, r5) <- readsPrec 0 r4
            , ("}", r6) <- lex r5]) r

test :: (:*:)
instance (Read a, Read b) => Read ((:*:) a b) where
    readsPrec p0 r =
        readParen (p0 > 10) (\r0 ->
            [ ((:*:) x1 x2, r5)
            | ("(", r1) <- lex r0
            , (":*:", r2) <- lex r1
            , (")", r3) <- lex r2
            , (x1, r4) <- readsPrec 11 r3
            , (x2, r5) <- readsPrec 11 r4]) r

-}
import Data.List
import Data.Derive.DSL.HSE
import qualified Language.Haskell as H

-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeRead :: Derivation
makeRead = derivationCustomDSL "Read" custom $
    List [Instance ["Read"] "Read" (List [App "InsDecl" (List [App
    "FunBind" (List [List [App "Match" (List [App "Ident" (List [
    String "readsPrec"]),List [App "PVar" (List [App "Ident" (List [
    Concat (List [String "p",ShowInt (Int 0)])])]),App "PVar" (List [
    App "Ident" (List [String "r"])])],App "Nothing" (List []),App
    "UnGuardedRhs" (List [Fold (App "InfixApp" (List [Head,App
    "QVarOp" (List [App "UnQual" (List [App "Symbol" (List [String
    "++"])])]),Tail])) (List [Application (List [App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "readParen"])])]),App
    "SpliceExp" (List [App "ParenSplice" (List [App "App" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String
    "bracket"])])]),App "Lit" (List [App "Int" (List [Int 0])])])])]),
    App "Paren" (List [App "Lambda" (List [List [App "PVar" (List [App
    "Ident" (List [Concat (List [String "r",ShowInt (Int 0)])])])],App
    "SpliceExp" (List [App "ParenSplice" (List [App "App" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String "comp"]
    )])]),App "Lit" (List [App "Int" (List [Int 0])])])])])])]),App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String "r"])])
    ])]),Application (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "readParen"])])]),App "SpliceExp" (List [App
    "ParenSplice" (List [App "App" (List [App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "bracket"])])]),App
    "Lit" (List [App "Int" (List [Int 1])])])])]),App "Paren" (List [
    App "Lambda" (List [List [App "PVar" (List [App "Ident" (List [
    Concat (List [String "r",ShowInt (Int 0)])])])],App "SpliceExp" (
    List [App "ParenSplice" (List [App "App" (List [App "Var" (List [
    App "UnQual" (List [App "Ident" (List [String "comp"])])]),App
    "Lit" (List [App "Int" (List [Int 1])])])])])])]),App "Var" (List
    [App "UnQual" (List [App "Ident" (List [String "r"])])])]),
    Application (List [App "Var" (List [App "UnQual" (List [App
    "Ident" (List [String "readParen"])])]),App "SpliceExp" (List [App
    "ParenSplice" (List [App "App" (List [App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "bracket"])])]),App
    "Lit" (List [App "Int" (List [Int 2])])])])]),App "Paren" (List [
    App "Lambda" (List [List [App "PVar" (List [App "Ident" (List [
    Concat (List [String "r",ShowInt (Int 0)])])])],App "SpliceExp" (
    List [App "ParenSplice" (List [App "App" (List [App "Var" (List [
    App "UnQual" (List [App "Ident" (List [String "comp"])])]),App
    "Lit" (List [App "Int" (List [Int 2])])])])])])]),App "Var" (List
    [App "UnQual" (List [App "Ident" (List [String "r"])])])]),App
    "List" (List [List []])])]),App "BDecls" (List [List []])])]])])])
    ]
-- GENERATED STOP

custom = customSplice splice

getCtor d i = dataDeclCtors (snd d) !! fromIntegral i
hasFields c = any ((/=) "" . fst) $ ctorDeclFields c

splice :: FullDataDecl -> Exp -> Exp
splice d (H.App x (H.Lit (H.Int y))) | x ~= "bracket" =
    if hasFields $ getCtor d y
    then con "False"
    else Paren $ InfixApp (var "p0") (QVarOp $ UnQual $ Symbol ">") (H.Lit $ H.Int 10)

splice d (H.App x (H.Lit (H.Int y))) | x ~= "comp" =
    if hasFields c then readFields c else readCtor c
    where c = getCtor d y


readCtor :: CtorDecl -> Exp
readCtor c =
    ListComp (Tuple [cpat, var ('r':show (cn+1))]) $ {- FIXME: serious bug here! -} reverse $
        QualStmt (Generator sl (PTuple [PLit $ H.String $ ctorDeclName c, pVar "r1"]) (var "lex" `H.App` var "r0")) :
        [QualStmt $ Generator sl
            (PTuple [pVar $ v 'x' 0, pVar $ v 'r' 1])
            (apps (var "readsPrec") [H.Lit $ H.Int 11, var $ v 'r' 0])
            | i <- [1..cn], let v c j = c : show (i+j)]
    where
        cn = ctorDeclArity c
        cpat = apps (con $ ctorDeclName c) $ map (var . ('x':) . show) [1..cn]


readFields :: CtorDecl -> Exp
readFields _ = var "todo"        


{-

read' dat = [instance_default "Read" dat [funN "readsPrec" [sclause [vr "p0", vr "r"] body]]]
    where
        body = (++::) [ readit ctr | ctr <- dataCtors dat ]

readit ctr = case ctorFields ctr of [] -> norm
                                    fl -> flds fl
    where
        norm = lK "readParen"
               [vr "p0" >: lit (10::Integer),
                "r0" ->: runComp (pName . foldr (.) id (map (pRead 11) (ctv ctr 'x'))) (ctp ctr 'x'),
                l0 "r"]

        flds f = lK "readParen" 
                 [false,
                  "r0" ->: runComp (pName . pLex "{" .
                                    foldr (.) id (intersperse (pLex ",")
                                                  (zipWith pField (ctv ctr 'x') f)) .
                                    pLex "}") (ctp ctr 'x'),
                  l0 "r"]

        runComp fn ex = CompE $ fn (\k -> [ NoBindS (tup [ex, vrn 'r' k]) ]) 0

        pArse pat fun ct k = BindS (tup [pat, vrn 'r' (k+1)]) (AppE fun (vrn 'r' k)) : ct (k+1)

        pLex pat = pArse (lit pat) (l0 "lex")

        name = ctorName ctr

        pName | isAlpha (head name) || head name == '_' = pLex name
              | otherwise                               = pLex "(" . pLex name . pLex ")"

        pRead pc pat = pArse pat (l1 "readsPrec" (lit (pc :: Integer)))

        pField pat fld = pLex fld . pLex "=" . pRead 0 pat
-}
