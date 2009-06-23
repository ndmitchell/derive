
module Derive.Derivation(wantDerive, performDerive, writeDerive) where

import System.IO
import System.IO.Unsafe
import Language.Haskell
import Control.Monad
import Data.List
import Derive.Utils
import Derive.Flags
import Data.Derive.All
import Data.Derive.Internal.Derivation


---------------------------------------------------------------------
-- WHAT DO YOU WANT TO DERIVE

wantDerive :: [Flag] -> (String, Module) -> [(String, DataDecl)]
wantDerive flag (str,modu) = nub $ wantDeriveFlag flag decls ++ wantDeriveAnnotation str decls
    where decls = filter isDataDecl $ moduleDecls modu


wantDeriveFlag :: [Flag] -> [DataDecl] -> [(String, DataDecl)]
wantDeriveFlag flags decls = [(x, d) | Derive xs <- flags, x <- xs, d <- decls]


-- find annotations by looking for {-! !-} parts, and matching up the source loc
wantDeriveAnnotation :: String -> [DataDecl] -> [(String, DataDecl)]
wantDeriveAnnotation src decls = [(d, decl) | (pos, ds) <- annotations, let decl = match pos, d <- ds]
    where
        annotations :: [((Int,Int), [String])]
        annotations = [((i,j),parse xs) | (i,s) <- zip [1..] $ lines src, (j,'{':'-':'!':xs) <- zip [1..] $ tails s]
        
        parse :: String -> [String]
        parse = words . reps ',' ' ' . takeWhile (/= '-')
        
        match :: (Int,Int) -> DataDecl
        match p = head [d | d <- reverse decls, let sl = dataDeclSrcLoc d, (srcLine sl, srcColumn sl) < p]


---------------------------------------------------------------------
-- ACTUALLY DERIVE IT

performDerive :: ModuleName -> [(String, DataDecl)] -> [String]
performDerive modu = concatMap ((:) "" . f)
    where
        f (name,dat) =
            case d (modu,dat) of
                Left x -> unsafePerformIO $ let res = msg x in hPutStrLn stderr res >> return ["-- " ++ res]
                Right x -> concatMap (lines . prettyPrint) x
            where d = head $ [op | Derivation n op <- derivations, n == name] ++ 
                             error (msg "Unknown derivation")
                  msg x = "Deriving " ++ name ++ " for " ++ dataDeclName dat ++ ": " ++ x


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



{-








import System.Console.GetOpt
import System.Environment
import System.Directory
import System.Exit
import System.Cmd
import System.FilePath
import System.Random
import System.IO
import Control.Monad
import Data.Maybe
import Data.List
import Data.Char
import Data.Int


data Flag = Version | Help | Output String | Import String | Module String | Use String
          | Append | Derive [String] | KeepTemp | NoOpts
            deriving (Eq, Show)


options :: [OptDescr Flag]
options =
 [ Option "v"  ["version"] (NoArg Version)          "show version number"
 , Option "h?" ["help"]    (NoArg Help)             "show help message"
 , Option "o"  ["output"]  (ReqArg Output "FILE")   "output FILE"
 , Option "i"  ["import"]  (OptArg (Import . fromMaybe "") "MODULE") "add an import statement"
 , Option "u"  ["use"]  (ReqArg Use "MODULE") "use additional derivations"
 , Option "m"  ["module"]  (ReqArg Module "MODULE") "add a module MODULE where statement"
 , Option "a"  ["append"]  (NoArg Append)           "append the result to the file"
 , Option "d"  ["derive"]  (ReqArg splt "DERIVES") "things to derive for all types"
 , Option "k"  ["keep"]    (NoArg KeepTemp)         "keep temporary file"
 , Option "n"  ["no-opts"] (NoArg NoOpts)           "ignore the file options"
 ]
 where
    splt = Derive . words . map (\x -> if x == ',' then ' ' else x)


getOpts :: IO ([Flag], [String])
getOpts = do
    args <- getArgs
    case getOpt Permute options args of
        (o,n,[]  ) | Version `elem` o -> putStrLn "Derive 0.1, (C) Neil Mitchell & Stefan O'Rear 2006-2008" >> exitSuccess
                   | Help `elem` o    -> putStr useage >> exitSuccess
                   | null n           -> putStr ("no files specified\n" ++ useage) >> exitSuccess
                   | otherwise        -> return (o, n)
        (_,_,errs) -> hPutStr stderr (concat errs ++ useage) >> exitFailure
    where
        useage = usageInfo "Usage: derive [OPTION...] files..." options
        exitSuccess = exitWith ExitSuccess

main :: IO ()
main = do
    (flags,files) <- getOpts
    fles <- mapM pickFile files
    mapM_ (mainFile flags) (catMaybes fles)
    when (any isNothing fles) exitFailure


pickFile :: FilePath -> IO (Maybe FilePath)
pickFile orig = f [orig, orig <.> "hs", orig <.> "lhs"]
    where
        f [] = hPutStrLn stderr ("Error, file not found: " ++ orig) >> return Nothing
        f (x:xs) = do
            b <- doesFileExist x
            if b then return $ Just x else f xs

appendMsg :: String
appendMsg = "--------------------------------------------------------\n" ++
            "-- DERIVES GENERATED CODE\n" ++
            "-- DO NOT MODIFY BELOW THIS LINE\n" ++
            "-- CHECKSUM: "


-- delete the end of a file with the appendMsg and a correct hash
-- make sure there are at least 4 blank lines at the end
-- return True for warning
dropAppend :: String -> (String,Bool)
dropAppend xs = f 0 xs
    where
        f i ys | appendMsg `isPrefixOf` ys =
                if hashString (filter (/= '\r') rest) == chk
                then f i []
                else (ys ++ "\n\n\n\n", True)
            where (chk, rest) = span isDigit $ drop (length appendMsg) ys

        f i [] = (replicate (4 - i) '\n', False)
        f i ('\n':ys) = add '\n' (f (i+1) ys)
        f _ (y:ys) = add y (f 0 ys)

        add c ~(cs,b) = (c:cs,b)


-- note: need to be careful that on Windows we don't convert \n files into
--       \r\n files
mainFile :: [Flag] -> FilePath -> IO ()
mainFile flags file = do
    file <- canonicalizePath file
    (fileflags,pragmas,modname,datas,reqs) <- parseFile flags file
    let devs = ["'\\n': $( _derive_string_instance make" ++ cls ++ " ''" ++ ctor ++ " )"
               | (ctor,cls) <- reqs]

    let hscode x = "{ -# OPTIONS_GHC -fth -fglasgow-exts -w #- }\n" ++
                   unlines pragmas ++
                   "module " ++ modname ++ " where\n" ++
                   "import Data.DeriveTH\n" ++
                   "import Data.Derive.All\n" ++
                   concat [ "import " ++ x ++ "\n" | Use x <- flags ] ++
                   datas ++ "\n" ++
                   "main = Prelude.writeFile " ++ show x ++ " $\n" ++
                   "    Prelude.unlines [" ++ concat (intersperse ", " devs) ++ "]\n"

    -- note: Wrong on Hugs on Windows
    tmpdir <- getTemporaryDirectory
    b <- doesDirectoryExist tmpdir
    tmpdr <- return $ if b && KeepTemp `notElem` flags then tmpdir else ""

    (hsfile, hshndl) <- openTempFileLocal tmpdr "Temp.hs"
    (txfile, txhndl) <- openTempFileLocal tmpdr "Temp.txt"
    hClose txhndl

    hPutStr hshndl $ hscode txfile
    hClose hshndl

    let incdir = dropTrailingPathSeparator $ joinPath $ reverse $
                 drop (1 + length (filter (== '.') modname)) $ reverse $ splitPath file
        cmd = "ghc -w -i. -i\"" ++ incdir ++ "\" -e " ++ modname ++ ".main " ++ hsfile
    code <- system cmd
    when (code /= ExitSuccess) $ do
        putStrLn "Failed to generate the code"
        exitWith code

    res <- readFile' txfile
    when (KeepTemp `notElem` flags) $ do
        removeFile hsfile
        removeFile txfile

    flgs <- return $ fileflags ++ flags
    if Append `elem` flgs then do
        src <- readFileBinary file
        let (src2,c) = dropAppend src
            ans = (if '\r' `elem` src then windowsNewLine else id)
                  (src2 ++ if null res then "" else appendMsg ++ hashString res ++ "\n" ++ res)
        when c $ putStrLn "Warning, Checksum does not match, please edit the file manually"
        writeFileBinary file ans
     else do
        let modline = concat $ take 1 ["module " ++ x ++ " where\n" | Module x <- flgs]
            impline = unlines ["import " ++ if null i then modname else i | Import i <- flgs]
            answer = modline ++ impline ++ res

        case [x | Output x <- flgs] of
             [] -> putStr answer
             (x:_) -> writeFile x answer


-- return the flags, a string that is the data structures only (including Typeable, Data)
-- and a set of derivation names with types

-- first disguard blank lines and lines which are -- comments
-- next find all lines which start a section, i.e. have something in column 0
-- group lines so every line starts at column 1
-- look for newtype, data etc.
-- look for deriving
parseFile :: [Flag] -> FilePath -> IO ([Flag], [String], String, String, [(String,String)])
parseFile flags file = do
        src <- liftM lines $ readFile file
        optns <- if NoOpts `elem` flags then return [] else parseOptions src
        pragmas <- return $ parsePragmas src
        modname <- parseModname src
        let deriv = concat [x | Derive x <- flags ++ optns]
        (decl,req) <- return $ unzip $ concatMap (checkData deriv) $ joinLines $
                               map dropComments $ filter (not . isBlank) src
        return (optns, pragmas, modname, unlines decl, concat req)
    where
        parsePragmas (x:xs)
            | "{ -#" `isPrefixOf` x2 && "#- }" `isSuffixOf` x2 = x2 : parsePragmas xs
            | null x2 = parsePragmas xs
            where x2 = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace x
        parsePragmas _ = []

        parseOptions (x:xs)
            | "{ -# OPTIONS_DERIVE " `isPrefixOf` x = do
                    a <- readOptions $ takeWhile (/= '#') $ drop 19 x
                    b <- parseOptions xs
                    return $ a ++ b
            | "{ -# OPTIONS" `isPrefixOf` x = parseOptions xs
        parseOptions _ = return []

        readOptions x = case getOpt Permute options (words x) of
                            (a,_,ns) -> mapM_ putStr ns >> return a


        parseModname (x:_) | "module " `isPrefixOf` x = return $ takeWhile f $ dropWhile isSpace $ drop 6 x
            where f y = not (isSpace y) && y `notElem` "("
        parseModname (_:ys) = parseModname ys
        parseModname [] = putStrLn "Error, module name not detected" >> return "Main"


        isBlank x = null x2 || "--" `isPrefixOf` x2
            where x2 = dropWhile isSpace x

        dropComments ('-':'-':_) = []
        dropComments (x:xs) = x : dropComments xs
        dropComments [] = []

        joinLines (x1:x2:xs) | col1 x1 && not (col1 x2) = joinLines ((x1 ++ x2) : xs)
            where col1 = null . takeWhile isSpace
        joinLines (x:xs) = x : joinLines xs
        joinLines [] = []

        checkData extra x
                | keyword `elem` ["data","newtype"] = [(x, map ((,) name) req)]
                | keyword `elem` ["type","import"] = [(x,[])]
                | otherwise = []
            where
                keyword = takeWhile (not . isSpace) x
                name = parseName $ drop (length keyword) x
                req = nub $ extra ++ parseDeriving x


        -- which derivings have been requested
        -- find all things inside { -! !- } and 'words' them
        parseDeriving :: String -> [String]
        parseDeriving x = words $ f False x
            where
                f _ ('{':'-':'!':xs) = ' ' : f True  xs
                f _ ('!':'-':'}':xs) = ' ' : f False xs
                f b (y:ys) = [if y == ',' then ' ' else y | b] ++ f b ys
                f _ [] = []


        -- if there is a =>, its just after that
        -- if there isn't, then its right now
        -- if the => is after =, then ignore
        parseName x = if "=>" `isPrefixOf` b
                      then parseName (drop 2 b)
                      else head (words a)
            where (a,b) = break (== '=') x


hashString :: String -> String
hashString = show . abs . foldl f 0 . filter (not . isSpace)
    where
        f :: Int32 -> Char -> Int32
        f x y = x * 31 + fromIntegral (ord y)


-- Note: openTempFile is not available on Hugs, which sucks
openTempFileLocal :: FilePath -> String -> IO (FilePath, Handle)
openTempFileLocal dir template = do
    i <- randomRIO (1000::Int,9999)
    let (file,ext) = splitExtension template
        s = dir </> (file ++ show i) <.> ext
    b <- doesFileExist s
    if b then openTempFileLocal dir template else do
        h <- openFile s ReadWriteMode
        return (s, h)


readFile' :: FilePath -> IO String
readFile' file = do
    h <- openFile file ReadMode
    res <- hGetContents h
    length res `seq` return ()
    hClose h
    return res


readFileBinary :: FilePath -> IO String
readFileBinary file = do
    h <- openBinaryFile file ReadMode
    hGetContents h


writeFileBinary :: FilePath -> String -> IO ()
writeFileBinary file s = do
    h <- openBinaryFile file WriteMode
    hPutStr h s
    hClose h


windowsNewLine :: String -> String
windowsNewLine ('\r':'\n':xs) = '\r':'\n': windowsNewLine xs
windowsNewLine ('\n':xs) = '\r':'\n': windowsNewLine xs
windowsNewLine (x:xs) = x : windowsNewLine xs
windowsNewLine [] = []

-}
