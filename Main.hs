
module Main(main) where

import Language.Haskell
import Derive.Derivation
import Derive.Generate
import Derive.Test
import Derive.Flags


main :: IO ()
main = do
    (flags,files) <- getFlags
    if Test `elem` flags then
        test
     else if Generate `elem` flags then
        generate
     else if null files then
        putStr $ "No files specified\n" ++ flagInfo
     else
        mapM_ (mainFile flags) files


mainFile :: [Flag] -> FilePath -> IO ()
mainFile flags file = do
    src <- readFile file
    let modu = unParseOk $ parseFileContents src -- FIXME: with this file mode
    flags <- return $ foldl addFlags flags
        [(sl,words x) | OptionsPragma sl (Just (UnknownTool "DERIVE")) x <- modulePragmas modu]
    let res = performDerive (moduleName modu) $ wantDerive flags (src,modu)
    writeDerive file (moduleName modu) flags res
