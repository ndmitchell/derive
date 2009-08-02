
module Derive.Test(test) where

import Derive.Utils
import Language.Haskell.Exts
import Data.Derive.DSL.HSE
import Data.DeriveDSL
import Control.Monad
import Data.Maybe
import Data.List
import System.FilePath
import System.Directory
import System.Cmd
import System.Exit
import Control.Arrow
import Data.Char
import Data.Derive.All
import Data.Derive.Internal.Derivation



listType :: Decl
listType = DataDecl sl DataType [] (Ident "[]") [UnkindedVar $ Ident "a"]
    [QualConDecl sl [] [] (ConDecl (Ident "[]") [])
    ,QualConDecl sl [] [] (ConDecl (Ident "Cons")
        [UnBangedTy (TyVar (Ident "a"))
        ,UnBangedTy (TyApp (TyCon (UnQual (Ident "List"))) (TyVar (Ident "a")))])]
    []


-- test each derivation
test :: IO ()
test = do
    decls <- fmap (filter isDataDecl . moduleDecls) $ readHSE "Data/Derive/Internal/Test.hs"

    -- check the test bits
    let ts = ("[]",listType) : map (dataDeclName &&& id) decls
    mapM_ (testFile ts) derivations

    -- check the $(derive) bits
    putStrLn "Type checking examples"
    let name = "AutoGenerated_Test.hs"
    srcs <- sequence [readSrc $ "Data/Derive" </> derivationName d <.> "hs" | d <- derivations]
    writeFile name $ unlines $ autoTest srcs decls derivations
    res <- system $ "runhaskell " ++ name
    when (res /= ExitSuccess) $ error "Failed to typecheck results"


testFile :: [(String,Decl)] -> Derivation -> IO ()
testFile types (Derivation name op) = do
    putStrLn $ "Testing " ++ name
    src <- readSrc $ "Data/Derive/" ++ name ++ ".hs"
    forM_ (srcTest src) $ \(typ,res) -> do
        let t = fromMaybe (error $ "wanting type: " ++ typ) $ lookup typ types
        let Right r = op (ModuleName "Example", t)
        when (not $ r `outEq` res) $
            error $ "Results don't match!\nExpected:\n" ++ showOut res ++ "\nGot:\n" ++ showOut r ++ "\n\n" ++ detailedNeq res r

detailedNeq as bs | na /= nb = "Lengths don't match, " ++ show na ++ " vs " ++ show nb
    where na = length as ; nb = length bs

detailedNeq as bs = "Mismatch on line " ++ show i ++ "\n" ++ show a ++ "\n" ++ show b
    where (i,a,b) = head $ filter (\(i,a,b) -> a /= b) $ zip3 [1..] (noSl as) (noSl bs)


ignore = ["ArbitraryOld","Serial","BinaryDefer","EnumCyclic","Ref","PlateTypeable","LazySet"]

autoTest :: [Src] -> [DataDecl] -> [Derivation] -> [String]
autoTest ss ts ds =
    ["{-# LANGUAGE TemplateHaskell,FlexibleInstances,MultiParamTypeClasses #-}"
    ,"{-# OPTIONS_GHC -Wall -fno-warn-missing-fields #-}"
    ,"import Data.DeriveTH"
    ,"import Derive.TestInstances()"] ++
    [prettyPrint i | s <- ss2, i <- srcImportStd s] ++
    ["main :: IO ()"
    ,"main = putStrLn \"Type checking successful\""] ++
    [prettyPrint t | t <- ts2] ++
    ["$(derives [make" ++ derivationName d ++ "] " ++ types ++ ")" | d <- ds2]
    where
        types = "[" ++ intercalate "," ["''" ++ dataDeclName t | t <- ts2] ++ "]"
        ts2 = filter (not . isBuiltIn) ts
        ds2 = filter (flip notElem ignore . derivationName) $ order ds
        ss2 = filter (flip notElem ignore . srcName) ss

isBuiltIn x = dataDeclName x `elem` ["Bool","Either"]



order :: [Derivation] -> [Derivation]
order ds = yes ++ no
    where (yes,no) = partition (flip elem ["Eq","Typeable"] . derivationName) ds
