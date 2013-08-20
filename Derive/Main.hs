
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
    let parse = fromParseResult . parseFileContentsWithMode defaultParseMode{parseFilename=file,extensions=extension}
        real = parse src
        mine = parse $ uncomment src
    flags <- return $ foldl addFlags flags
        [(sl,words x) | OptionsPragma sl (Just (UnknownTool "DERIVE")) x <- modulePragmas mine]
    let res = performDerive derivations mine $ wantDerive flags real mine
    writeDerive file (moduleName mine) flags res


uncomment :: String -> String
uncomment ('{':'-':'!':xs) = ' ':' ':' ':uncomment xs
uncomment ('!':'-':'}':xs) = ' ':' ':' ':uncomment xs
uncomment (x:xs) = x:uncomment xs
uncomment [] = []


-- Taken from HLint, update occasionally
extension =
    [OverlappingInstances,UndecidableInstances,IncoherentInstances,RecursiveDo
    ,ParallelListComp,MultiParamTypeClasses,FunctionalDependencies
    ,Rank2Types,RankNTypes,PolymorphicComponents,ExistentialQuantification,ScopedTypeVariables
    ,ImplicitParams,FlexibleContexts,FlexibleInstances,EmptyDataDecls
    ,KindSignatures,BangPatterns,TypeSynonymInstances,TemplateHaskell
    ,ForeignFunctionInterface,Generics,NamedFieldPuns,PatternGuards
    ,GeneralizedNewtypeDeriving,ExtensibleRecords,RestrictedTypeSynonyms,HereDocuments
    ,MagicHash,TypeFamilies,StandaloneDeriving,UnicodeSyntax,PatternSignatures,UnliftedFFITypes
    ,LiberalTypeSynonyms,TypeOperators,RecordWildCards,RecordPuns,DisambiguateRecordFields
    ,OverloadedStrings,GADTs,MonoPatBinds,RelaxedPolyRec,ExtendedDefaultRules,UnboxedTuples
    ,DeriveDataTypeable,ConstrainedClassMethods,PackageImports,ImpredicativeTypes
    ,NewQualifiedOperators,PostfixOperators,QuasiQuotes,ViewPatterns]
