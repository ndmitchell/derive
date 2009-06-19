
module Derive.Utils where

import Data.Derive.DSL.HSE
import Data.List
import qualified Data.ByteString.Char8 as BS
import System.Directory
import System.IO
import Control.Monad


data Src = Src
    {srcModule :: Maybe String
    ,srcPackage :: Maybe String
    ,srcExample :: Maybe [Decl]
    ,srcTest :: [(String,[Decl])]
    }

nullSrc = Src Nothing Nothing Nothing []


readSrc :: FilePath -> IO Src
readSrc file = do
    src <- readFile' file
    decls <- case parseFileContents $ unlines $ ("module Example where":) $ takeWhile (/= "-}") $ drop 1 $ lines src of
        ParseOk x -> return $ moduleDecls x
        ParseFailed pos msg -> do putStrLn $ "Failed to parse " ++ file ++ ": " ++ prettyPrint pos ++ " " ++ msg; return []
    return $ foldl f nullSrc
        [ (pragma,extra,xs)
        | UnknownDeclPragma _ pragma extra:real <- tails decls
        , let xs = takeWhile (not . isUnknownDeclPragma) real ]
    where
        f src (pragma,extra,bod)
            | pragma == "MODULE" = src{srcModule = Just extra}
            | pragma == "PACKAGE" = src{srcPackage = Just extra}
            | pragma == "EXAMPLE" = src{srcExample = Just bod}
            | pragma == "TEST" = src{srcTest = (extra,bod) : srcTest src}


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
