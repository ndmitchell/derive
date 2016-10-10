
module Derive.Generate(generate) where

import Language.Haskell.Exts
import Data.DeriveDSL
import Derive.Utils
import Control.Monad
import Data.Maybe
import System.FilePath
import System.Directory
import Data.Char
import Data.List


evil = words "TTypeable Uniplate"

-- generate extra information for each derivation
generate :: IO ()
generate = do
    xs <- getDirectoryContents "src/Data/Derive"
    xs <- return $ sort [x | x <- xs, takeExtension x == ".hs", x /= "All.hs", takeBaseName x `notElem` evil]
    lis <- mapM generateFile $ map ("src/Data/Derive" </>) xs
    let names = map dropExtension xs
        n = maximum $ map length names
    writeGenerated "src/Data/Derive/All.hs" $
        ["import Data.Derive." ++ x ++ replicate (4 + n - length x) ' ' ++ "as D" | x <- names] ++
        ["derivations :: [Derivation]"
        ,"derivations = [make" ++ concat (intersperse ",make" names) ++ "]"]
    writeGenerated "README.md" $ ["-->",""] ++ lis ++ ["","<!--"]
    writeGenerated "derive.cabal" $ map ("        Data.Derive."++) names


-- return the Documentation string
generateFile :: FilePath -> IO String
generateFile file = do
    let name = takeBaseName file
    putStrLn $ "Generating " ++ name
    src <- readSrc file
    when (isJust $ srcExample src) $ do
        let dsl = fromMaybe (error $ "Couldn't derive example for " ++ name) $
                            deriveDSL $ fromJust $ srcExample src
        writeGenerated file $
            [""
            ,"import Data.Derive.DSL.DSL"
            ,"import Data.Derive.Internal.Derivation"
            ,""
            ,"make" ++ name ++ " :: Derivation"
            ] ++ (if srcCustom src then
                ["make" ++ name ++ " = derivationCustomDSL " ++ show name ++ " custom $"]
            else
                ["make" ++ name ++ " = derivationDSL " ++ show name ++ " dsl" ++ name
                ,""
                ,"dsl" ++ name ++ " ="
            ]) ++
            map (replicate 4 ' ' ++) (wrap 66 $ show dsl)

        let inst = dynamicDSL dsl
            instFile = takeDirectory file </> "Instance" </> name <.> "hs"
        b <- doesFileExist instFile

        if not (srcCustom src) && isJust inst then do
            writeGenerated instFile $
                ["{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}"] ++
                ["","module Data.Derive.Instance." ++ name ++ " where",""] ++
                (map prettyPrint $ srcImportStd src) ++
                ["import Data.Derive.Internal.Instance",""] ++
                (map prettyPrint $ fromJust inst) ++ [""]
         else when b $
            error $ "Previously generated dynamic instance can not be regenerated, " ++ name

    let imp = listToMaybe $ srcImport src
    return $
        "* **[" ++ name ++ "](" ++ instUrl name imp ++ ")**" ++
        concat [" - from the library [" ++ pkg ++ "](" ++ pkgUrl pkg ++ ")" | Just imp <- [imp], let pkg = fromMaybe "base" $ importPkg imp]

pkgUrl x = "http://hackage.haskell.org/package/" ++ x

instUrl name Nothing = "http://hackage.haskell.org/packages/archive/derive/latest/doc/html/Data-Derive-" ++ name ++ ".html"
instUrl name (Just x) = "http://hackage.haskell.org/packages/archive/" ++ pkgName ++ "/" ++ pkgVersion ++ "/doc/html/" ++ modu ++ ".html#t%3A" ++ nam
    where
        (a,b) = break (== '-') $ fromMaybe "base" $ importPkg x
        pkgName = a
        pkgVersion = if null b then "latest" else tail b
        modu = reps '.' '-' $ prettyPrint $ importModule x
        nam = case importSpecs x of Just (ImportSpecList _ False (IAbs _ _ y:_)) -> prettyPrint y ; _ -> name


wrap :: Int -> String -> [String]
wrap n = f . lexemes
    where
        f [] = []
        f (x:xs) = [reverse $ dropWhile isSpace $ reverse $ concat $ x:a] ++ f (dropWhile (all isSpace) b)
            where (a,b) = thisLine (n - length x) xs

        thisLine i [] = ([], [])
        thisLine i (x:xs) | j > i = ([], x:xs)
                          | otherwise = (x:a, b)
            where j = length x
                  (a,b) = thisLine (i - j) xs


lexemes :: String -> [String]
lexemes [] = []
lexemes x = a : lexemes b
    where (a,b) = lexeme x


lexeme :: String -> (String, String)
lexeme xs@(x:_) | isAlpha x = span isAlpha xs
lexeme ('\"':xs) = let (a,b) = f xs in ('\"':a,b)
    where f ('\\':x:xs) = let (a,b) = f xs in ('\\':x:a,b)
          f ('\"':xs) = ("\"",xs)
          f (x:xs) = let (a,b) = f xs in (x:a,b)
          f [] = ([],[])
lexeme (x:xs) = ([x], xs)
