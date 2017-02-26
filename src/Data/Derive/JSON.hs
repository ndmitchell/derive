-- |
-- Copyright:   (c) Bertram Felgenhauer 2009
-- License:     BSD3
-- Stability:   experimental
-- Portability: portable
--
-- Derive 'Text.JSON' instances.
--
-- Unlike Text.JSON.Generics, single constructor types are /not/ handled
-- specially. Every value is encoded as an object with a single field,
-- with the constructor name as key and the values as its contents.
--
-- If the constructor is a record, the contents is an Object with the
-- field names as keys. Otherwise, the contents is an array.

module Data.Derive.JSON (makeJSON) where

import qualified Language.Haskell as H
import Language.Haskell (
    Exp, Pat, Alt, CtorDecl, Decl, FullDataDecl, FieldDecl, Type, Stmt,
    (~=), var, pVar, con, strE, strP, apps, qname,
    ctorDeclFields, ctorDeclName, dataDeclCtors)

{-
import "json" Text.JSON
import Text.JSON.Types

example :: Custom
instance JSON a => JSON (Sample a) where
    readJSON (JSObject x)   = $(readJSON)
    readJSON _              = Error "..."
    showJSON (First)        = $(showJSON 0)
    showJSON (Second x1 x2) = $(showJSON 1)
    showJSON (Third x1)     = $(showJSON 2)
-}

-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeJSON :: Derivation
makeJSON = derivationCustomDSL "JSON" custom $
    List [Instance ["JSON"] "JSON" (App "Just" (List [List [App
    "InsDecl" (List [App "()" (List []),App "FunBind" (List [App "()"
    (List []),List [App "Match" (List [App "()" (List []),App "Ident"
    (List [App "()" (List []),String "readJSON"]),List [App "PParen" (
    List [App "()" (List []),App "PApp" (List [App "()" (List []),App
    "UnQual" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "JSObject"])]),List [App "PVar" (List [App "()" (
    List []),App "Ident" (List [App "()" (List []),String "x"])])]])])
    ],App "UnGuardedRhs" (List [App "()" (List []),App "SpliceExp" (
    List [App "()" (List []),App "ParenSplice" (List [App "()" (List [
    ]),App "Var" (List [App "()" (List []),App "UnQual" (List [App
    "()" (List []),App "Ident" (List [App "()" (List []),String
    "readJSON"])])])])])]),App "Nothing" (List [])]),App "Match" (List
    [App "()" (List []),App "Ident" (List [App "()" (List []),String
    "readJSON"]),List [App "PWildCard" (List [App "()" (List [])])],
    App "UnGuardedRhs" (List [App "()" (List []),App "App" (List [App
    "()" (List []),App "Con" (List [App "()" (List []),App "UnQual" (
    List [App "()" (List []),App "Ident" (List [App "()" (List []),
    String "Error"])])]),App "Lit" (List [App "()" (List []),App
    "String" (List [App "()" (List []),String "...",String "..."])])])
    ]),App "Nothing" (List [])])]])]),App "InsDecl" (List [App "()" (
    List []),App "FunBind" (List [App "()" (List []),MapCtor (App
    "Match" (List [App "()" (List []),App "Ident" (List [App "()" (
    List []),String "showJSON"]),List [App "PParen" (List [App "()" (
    List []),App "PApp" (List [App "()" (List []),App "UnQual" (List [
    App "()" (List []),App "Ident" (List [App "()" (List []),CtorName]
    )]),MapField (App "PVar" (List [App "()" (List []),App "Ident" (
    List [App "()" (List []),Concat (List [String "x",ShowInt
    FieldIndex])])]))])])],App "UnGuardedRhs" (List [App "()" (List []
    ),App "SpliceExp" (List [App "()" (List []),App "ParenSplice" (
    List [App "()" (List []),App "App" (List [App "()" (List []),App
    "Var" (List [App "()" (List []),App "UnQual" (List [App "()" (List
    []),App "Ident" (List [App "()" (List []),String "showJSON"])])]),
    App "Lit" (List [App "()" (List []),App "Int" (List [App "()" (
    List []),CtorIndex,ShowInt CtorIndex])])])])])]),App "Nothing" (
    List [])]))])])]]))]
-- GENERATED STOP

-- ^ 'Derivation' for 'JSON'

custom :: FullDataDecl -> [Decl ()] -> [Decl ()]
custom = customSplice splice

splice :: FullDataDecl -> Exp () -> Exp ()
splice d x | x ~= "readJSON" = mkRead d
splice d (H.App _ x (H.Lit _ (H.Int _ y _))) | x~= "showJSON" = mkShow d y
splice _ e = error $ "makeJSON: unrecognized splice: " ++ show e

------------------------------------------------------------------------------
-- showJSON

mkShow :: FullDataDecl -> Integer -> Exp ()
mkShow d y = let
    hasFields = any (not . null . fst) (ctorDeclFields c)
    c = dataDeclCtors (snd d) !! fromInteger y
    mkFields = if hasFields then mkShowRecordFields else mkShowPlainFields
  in
    mkJSObject $ H.List ()
        [H.Tuple () H.Boxed [strE (ctorDeclName c), mkFields (ctorDeclFields c)]]

mkShowPlainFields :: FieldDecl -> Exp ()
mkShowPlainFields fs = mkJSArray $ H.List ()
    [H.App () (var "showJSON") xi | xi <- vars "x" fs]

mkShowRecordFields :: FieldDecl -> Exp ()
mkShowRecordFields fs = mkJSObject $ H.List ()
    [ H.Tuple () H.Boxed [strE fn, H.App () (var "showJSON") xi]
    | ((fn, _), xi) <- zip fs (vars "x" fs)]

------------------------------------------------------------------------------
-- readJSON

mkRead :: FullDataDecl -> Exp ()
mkRead (_, d) = let
    readError = H.App () (con "Error") $ strE "malformed JSON for type ...: ..."
  in
    H.Case () (H.App () (var "fromJSObject") $ var "x") $
    map mkReadCtor (dataDeclCtors d) ++
    [H.Alt () (H.PWildCard ()) (H.UnGuardedRhs () readError) Nothing]

mkReadCtor :: CtorDecl -> Alt ()
mkReadCtor c = let
    cn = ctorDeclName c
    fs = ctorDeclFields c
    hasFields = any (not . null . fst) fs
    body | hasFields = mkReadRecord cn fs
         | otherwise = mkReadPlain cn fs
  in
    H.Alt () (H.PList () [H.PTuple () H.Boxed [strP cn, pVar "y"]])
         (H.UnGuardedRhs () body) Nothing

mkReadRecord :: String -> FieldDecl -> Exp ()
mkReadRecord cn fs = H.Do () $
    [H.Generator () (H.PApp () (qname "JSObject") [pVar "z"])
          (H.App () (var "return") $ var "y")] ++
    [H.LetStmt () $ H.BDecls () [H.PatBind () (pVar "d")
          (H.UnGuardedRhs () $ H.App () (var "fromJSObject") $ var "z")
          Nothing]] ++
    zipWith (mkReadRecordField cn) (pVars "x" fs) fs ++
    mkReadTrailer cn fs

mkReadRecordField :: String -> Pat () -> (String, Type ()) -> Stmt ()
mkReadRecordField cn xi (fn, _) = H.Generator () xi $
    apps (var "maybe") [
        H.App () (var "fail") $ strE (unwords ["readJSON: missing field", fn,
                                          "while decoding a", cn]),
        var "return",
        apps (var "lookup") [strE fn, var "d"]]

mkReadPlain :: String -> FieldDecl -> Exp ()
mkReadPlain cn fs = H.Do () $
    [H.Generator () (H.PApp () (qname "JSArray") [H.PList () (pVars "x" fs)])
        (H.App () (var "return") $ var "y")] ++
    mkReadTrailer cn fs

mkReadTrailer :: String -> FieldDecl -> [Stmt ()]
mkReadTrailer cn fs =
    [ H.Generator () yi (H.App () (var "readJSON") xi)
    | (xi, yi) <- zip (vars "x" fs) (pVars "y" fs)] ++
    [H.Qualifier () $ H.App () (var "return") $ apps (con cn) (vars "y" fs)]

------------------------------------------------------------------------------
-- utilites

mkJSObject :: Exp () -> Exp ()
mkJSObject e = H.App () (con "JSObject") (H.App () (var "toJSObject") e)

mkJSArray :: Exp () -> Exp ()
mkJSArray e = H.App () (con "JSArray") e

vars :: String -> FieldDecl -> [Exp ()]
vars pre fs = [var (pre ++ show i) | i <- [1..length fs]]

pVars :: String -> FieldDecl -> [Pat ()]
pVars pre fs = [pVar (pre ++ show i) | i <- [1..length fs]]
