
module Derive.Main(deriveMain) where

import Language.Haskell
import Data.Derive.All(Derivation)
import Derive.Derivation
import Derive.Generate
import Derive.Test
import Derive.Flags
import Data.List
import System.Directory


deriveMain :: [Derivation] -> IO ()
deriveMain derivations = do
    (flags,files) <- getFlags
    if Test `elem` flags then
        test
     else if Generate `elem` flags then
        generate
     else if Preprocessor `elem` flags then
        (if length files /= 3 then
            error $ "Expected to be invoked as a GHC preprocessor with 3 files, but got " ++ show (length files)
         else do
            copyFile (files !! 1) (files !! 2)
            mainFile derivations (Append:flags) (files !! 2)
         )
     else if null files then
        putStr $ "No files specified\n" ++ flagInfo
     else
        mapM_ (mainFile derivations flags) files


mainFile :: [Derivation] -> [Flag] -> FilePath -> IO ()
mainFile derivations flags file = do
    src <- readFile file
    src <- return $ unlines $ filter (not . isPrefixOf "#") $ lines src
    let parse = fromParseResult . parseFileContentsWithMode defaultParseMode{parseFilename=file,extensions=defaultExtensions}
        real = parse src
        mine = parse $ uncomment src :: Module SrcSpanInfo
    flags <- return $ foldl addFlags flags
        [(getPointLoc sl,words x) | OptionsPragma sl (Just (UnknownTool "DERIVE")) x <- modulePragmas mine]
    let blur = fmap (const ())
    let res = performDerive derivations (blur mine :: Module ()) $ wantDerive flags (blur real) (blur mine)
    writeDerive file (moduleName $ blur mine) flags res


uncomment :: String -> String
uncomment ('{':'-':'!':xs) = ' ':' ':' ':uncomment xs
uncomment ('!':'-':'}':xs) = ' ':' ':' ':uncomment xs
uncomment (x:xs) = x:uncomment xs
uncomment [] = []


-- Taken from HLint, update occasionally
defaultExtensions :: [Extension]
defaultExtensions = [e | e@EnableExtension{} <- knownExtensions] \\ map EnableExtension badExtensions

badExtensions =
    [Arrows -- steals proc
    ,TransformListComp -- steals the group keyword
    ,XmlSyntax, RegularPatterns -- steals a-b
    ,UnboxedTuples -- breaks (#) lens operator
    ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
    ,DoRec, RecursiveDo -- breaks rec
    ,TypeApplications -- HSE fails on @ patterns
    ]
