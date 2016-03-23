
module Derive.Flags(Flag(..), getFlags, addFlags, flagInfo) where

import System.Environment
import System.Console.GetOpt
import System.Directory
import Language.Haskell
import System.Exit
import System.IO
import Data.Maybe


data Flag = Version | Help | Output String | Import String | Modu String
          | Append | Derive [String] | NoOpts | Preprocessor | Test | Generate
            deriving (Eq, Show)


options :: [OptDescr Flag]
options =
    [Option "v"  ["version"]  (NoArg Version)          "show version number"
    ,Option "h?" ["help"]     (NoArg Help)             "show help message"
    ,Option "o"  ["output"]   (ReqArg Output "FILE")   "output FILE"
    ,Option "i"  ["import"]   (OptArg (Import . fromMaybe "") "MODULE") "add an import statement"
    ,Option "m"  ["module"]   (ReqArg Modu "MODULE")   "add a module MODULE where statement"
    ,Option "a"  ["append"]   (NoArg Append)           "append the result to the file"
    ,Option "d"  ["derive"]   (ReqArg splt "DERIVES")  "things to derive for all types"
    ,Option "n"  ["no-opts"]  (NoArg NoOpts)           "ignore the file options"
    ,Option "F"  ["preprocessor"] (NoArg Preprocessor) "operate as a GHC preprocessor with -pgmF"
    ,Option ""   ["test"]     (NoArg Test)             "run the test suite"
    ,Option ""   ["generate"] (NoArg Generate)         "perform code generation"
    ]
    where splt = Derive . words . map (\x -> if x == ',' then ' ' else x)


flagInfo = usageInfo "Usage: derive [OPTION...] files..." options


getFlags :: IO ([Flag], [String])
getFlags = do
    args <- getArgs
    case getOpt Permute options args of
        (o,n,[]  ) | Version `elem` o -> putStrLn "Derive 2.5.* (C) Neil Mitchell 2006-2013" >> exitSuccess
                   | Help `elem` o    -> putStr flagInfo >> exitSuccess
                   | Preprocessor `elem` o -> return (o,n)
                   | otherwise        -> do files <- mapM pickFile n; return (o, files)
        (_,_,errs) -> hPutStr stderr (concat errs ++ flagInfo) >> exitFailure
    where
        exitSuccess = exitWith ExitSuccess


pickFile :: FilePath -> IO FilePath
pickFile orig = f [orig, orig ++ ".hs", orig ++ ".lhs"]
    where
        f [] = error $ "File not found: " ++ orig
        f (x:xs) = do
            b <- doesFileExist x
            if b then return x else f xs


addFlags :: [Flag] -> (SrcLoc, [String]) -> [Flag]
addFlags flags (sl,xs)
    | NoOpts `elem` flags = flags
    | errs /= [] = error $ prettyPrint sl ++ "\n" ++ concat errs
    | otherwise = flags ++ a
    where (a,_,errs) = getOpt Permute options xs
