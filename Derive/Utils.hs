
module Derive.Utils where

import Data.Derive.DSL.HSE
import Data.List
import qualified Data.ByteString.Char8 as BS
import System.Directory
import System.IO
import Control.Monad
import Data.Char
import Data.Maybe


data Src = Src
    {srcModule :: Maybe String
    ,srcPackage :: Maybe String
    ,srcExample :: Maybe [Decl]
    ,srcTest :: [(String,[Decl])]
    }

nullSrc = Src Nothing Nothing Nothing []


readHSE :: FilePath -> IO Module
readHSE file = do
    src <- readFile' file
    src <- return $ takeWhile (/= "-}") $ drop 1 $ dropWhile (/= "{-") $
                    dropWhile (not . isPrefixOf "module ") $ lines src

    let mode = defaultParseMode{extensions=[MultiParamTypeClasses,TemplateHaskell,PackageImports]}
    return $ fromParseResult $ parseFileContentsWithMode mode $ unlines $ "module Example where":src


data Pragma = Example | Test String

asPragma :: Decl -> Maybe Pragma
asPragma (TypeSig _ [x] t)
    | x ~= "example" = Just Example
    | x ~= "test" = Just $ Test $ prettyPrint t
asPragma _ = Nothing


readSrc :: FilePath -> IO Src
readSrc file = do
    modu <- readHSE file
    return $ foldl f (foldl g nullSrc $ moduleImports modu)
        [ (p,xs)
        | p:real <- tails $ moduleDecls modu, Just p <- [asPragma p]
        , let xs = takeWhile (isNothing . asPragma) real ]
    where
        g src i = src{srcModule = Just $ prettyPrint $ importModule i, srcPackage = importPkg i}

        f src (Example,bod) = src{srcExample = Just bod}
        f src (Test x ,bod) = src{srcTest = srcTest src ++ [(x,bod)]}


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
