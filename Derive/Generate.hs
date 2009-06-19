
module Derive.Generate(generate) where

import Language.Haskell.Exts
import Data.DeriveDSL
import Derive.Utils
import Control.Monad
import Data.Maybe
import System.FilePath
import System.Directory


-- generate extra information for each derivation
generate :: IO ()
generate = do
    xs <- getDirectoryContents "Data/Derive"
    xs <- return [x | x <- xs, takeExtension x == ".hs", x /= "All.hs"]
    mapM_ generateFile $ map ("Data/Derive" </>) xs
    let names = map dropExtension xs
        n = maximum $ map length names
    writeGenerated "Data/Derive/All.hs"
        ["import Data.Derive." ++ x ++ replicate (4 + n - length x) ' ' ++ "as D" | x <- names]


generateFile :: FilePath -> IO ()
generateFile file = do
    let name = takeBaseName file
    putStrLn $ "Generating " ++ name
    src <- readSrc file
    when (isJust $ srcExample src) $ do
        let dsl = fromMaybe (error $ "Couldn't derive example for " ++ name) $
                            deriveDSL $ fromJust $ srcExample src
        let modu = "module Data.Derive." ++ name ++ " where"
            imp = "import Language.Haskell.TH.All"
            dslFoo = ("dsl" ++ name ++ " =") : map (replicate 4 ' ' ++) (wrap 66 $ show dsl)
            makeFoo = ("make" ++ name ++ " :: Derivation") : ("make" ++ name ++ " = undefined") : []
        writeGenerated file $ ["",modu,"",imp,""] ++ dslFoo ++ [""] ++ makeFoo

        let inst = dynamicDSL dsl
        when (isJust inst) $ do
            writeGenerated (takeDirectory file </> "Instance" </> name <.> "hs") $
                ["{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}"] ++
                ["","module Data.Derive.Instance." ++ name ++ " where",""] ++
                ["import " ++ x | Just x <- [srcModule src]] ++
                ["import Data.Derive.Internal.Instance",""] ++
                (map prettyPrint $ fromJust inst) ++ [""]


wrap :: Int -> String -> [String]
wrap n = f . words
    where
        f [] = []
        f (x:xs) = [unwords (x:a) | a /= []] ++ f b
            where (a,b) = thisLine (n - length x - 1) xs

        thisLine i [] = ([], [])
        thisLine i (x:xs) | j > i = ([], x:xs)
                          | otherwise = (x:a, b)
            where j = length x
                  (a,b) = thisLine (i - j - 1) xs
