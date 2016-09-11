
module Derive.Utils where

import Data.Derive.DSL.HSE
import Data.List
import qualified Data.ByteString.Char8 as BS
import System.Directory
import System.IO
import System.FilePath
import Control.Monad
import Data.Maybe


data Src = Src
    {srcName :: String
    ,srcImport :: [ImportDecl ()]
    ,srcExample :: Maybe [Decl ()]
    ,srcTest :: [(Type (),[Decl ()])]
    ,srcCustom :: Bool
    }

-- skip the importPkg bits
srcImportStd :: Src -> [ImportDecl ()]
srcImportStd y= [x{importPkg=Nothing} | x <- srcImport y]

nullSrc = Src "" [] Nothing [] False


readHSE :: FilePath -> IO (Module ())
readHSE file = do
    src <- readFile' file
    src <- return $ takeWhile (/= "-}") $ drop 1 $ dropWhile (/= "{-") $
                    dropWhile (not . isPrefixOf "module ") $ lines src

    let mode = defaultParseMode{extensions=map EnableExtension [MultiParamTypeClasses,FlexibleContexts,TemplateHaskell,PackageImports,TypeOperators]}
    return $ fmap (const ()) $ fromParseResult $ parseFileContentsWithMode mode $ unlines $ "module Example where":src


data Pragma = Example Bool | Test (Type ())

asPragma :: Decl () -> Maybe Pragma
asPragma (TypeSig _ [x] t)
    | x ~= "example" = Just $ Example $ prettyPrint t == "Custom"
    | x ~= "test" = Just $ Test t
asPragma _ = Nothing


readSrc :: FilePath -> IO Src
readSrc file = do
    modu <- readHSE file
    return $ foldl f nullSrc{srcName=takeBaseName file, srcImport=moduleImports modu}
        [ (p,xs)
        | p:real <- tails $ moduleDecls modu, Just p <- [asPragma p]
        , let xs = takeWhile (isNothing . asPragma) real ]
    where
        f src (Example x,bod) = src{srcExample = Just bod, srcCustom = x}
        f src (Test    x,bod) = src{srcTest = srcTest src ++ [(x,bod)]}


generatedStart = "-- GENERATED START"
generatedStop  = "-- GENERATED STOP"



writeGenerated :: FilePath -> [String] -> IO ()
writeGenerated file x = do
    src <- fmap lines $ readFile' file
    let pre = takeWhile (/= generatedStart) src
        post = drop 1 $ dropWhile (/= generatedStop) src
        src2 = pre ++ [generatedStart] ++ x ++ [generatedStop] ++ post
    when (src /= src2) $
        seq (length src2) $ writeBinaryFile file $ unlines src2


readFile' :: FilePath -> IO String
readFile' file = do
    b <- doesFileExist file
    if b then fmap BS.unpack $ BS.readFile file else return []


writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile file x = withBinaryFile file WriteMode (`hPutStr` x)


rep from to x = if x == from then to else x
reps from to = map (rep from to)
