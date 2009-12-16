
module Derive.Derivation(wantDerive, performDerive, writeDerive) where

import System.IO
import System.IO.Unsafe
import Language.Haskell
import Control.Monad
import Data.Maybe
import Data.List
import Derive.Utils
import Derive.Flags
import Data.Derive.All
import Data.Derive.Internal.Derivation


---------------------------------------------------------------------
-- WHAT DO YOU WANT TO DERIVE

wantDerive :: [Flag] -> (String, Module) -> [(Type, DataDecl)]
wantDerive flag (str,modu) = nub $ wantDeriveFlag flag decls ++ wantDeriveAnnotation str decls
    where decls = filter isDataDecl $ moduleDecls modu


wantDeriveFlag :: [Flag] -> [DataDecl] -> [(Type, DataDecl)]
wantDeriveFlag flags decls = [(TyCon $ UnQual $ Ident x, d) | Derive xs <- flags, x <- xs, d <- decls]


-- find annotations by looking for {-! !-} parts, and matching up the source loc
wantDeriveAnnotation :: String -> [DataDecl] -> [(Type, DataDecl)]
wantDeriveAnnotation src decls = [(d, decl) | (pos, ds) <- annotations, let decl = match pos, d <- ds]
    where
        annotations :: [((Int,Int), [Type])]
        annotations = [((i,j),parse xs) | (i,s) <- zip [1..] $ lines src, (j,'{':'-':'!':xs) <- zip [1..] $ tails s]
        
        parse :: String -> [Type]
        parse x = fromTyTuple $ fromParseResult $ parseType $ "(" ++ closeComment x ++ ")"

        closeComment ('!':'-':'}':xs) = ""
        closeComment ('-':'}':xs) = ""
        closeComment (x:xs) = x : closeComment xs
        closeComment [] = ""
        
        match :: (Int,Int) -> DataDecl
        match p = head [d | d <- reverse decls, let sl = dataDeclSrcLoc d, (srcLine sl, srcColumn sl) < p]


---------------------------------------------------------------------
-- ACTUALLY DERIVE IT

performDerive :: Module -> [(Type, DataDecl)] -> [String]
performDerive modu = concatMap ((:) "" . f)
    where
        tbl = concatMap g $ moduleDecls modu
        g x@(DataDecl _ _ _ name _ _ _) = [(prettyPrint name, x)]
        g x@(GDataDecl _ _ _ name _ _ _ _) = [(prettyPrint name, x)]
        g x@(TypeDecl _ name _ _) = [(prettyPrint name, x)]
        g _ = []
        grab x = fromMaybe (error $ "Can't access definition for: " ++ x) $ lookup x tbl

        f (name,dat) =
            case d name grab (moduleName modu,dat) of
                Left x -> unsafePerformIO $ let res = msg x in hPutStrLn stderr res >> return ["-- " ++ res]
                Right x -> concatMap (lines . prettyPrint) x
            where d = head $ [op | Derivation n op <- derivations, n == name2] ++ 
                             error (msg "Unknown derivation")
                  name2 = prettyPrint $ fst $ fromTyApps $ fromTyParen name
                  msg x = "Deriving " ++ name2 ++ " for " ++ dataDeclName dat ++ ": " ++ x


---------------------------------------------------------------------
-- WRITE IT BACK

writeDerive :: FilePath -> ModuleName -> [Flag] -> [String] -> IO ()
writeDerive file modu flags xs = do
    -- force the output first, ensure that we don't crash half way through
    () <- length (concat xs) `seq` return ()
    
    let append = Append `elem` flags
    let output = [x | Output x <- flags]

    let pre = take 1 ["module " ++ x ++ " where" | Modu x <- flags] ++
              ["import " ++ if null i then prettyPrint modu else i | Import i <- flags]

    when append $ do
        src <- readFile' file
        writeGenerated file xs

    forM output $ \o ->
        writeFile o $ unlines $ pre ++ xs

    when (not append && null output) $
        putStr $ unlines xs
