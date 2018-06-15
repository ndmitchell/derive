{-# LANGUAGE ViewPatterns #-}

module Derive.Derivation(wantDerive, performDerive, writeDerive) where

import System.IO
import System.IO.Unsafe
import Language.Haskell
import Control.Arrow
import Control.Monad
import Data.List
import Derive.Utils
import Derive.Flags
import Data.Derive.Internal.Derivation
import qualified Data.Map as Map


---------------------------------------------------------------------
-- WHAT DO YOU WANT TO DERIVE

wantDerive :: [Flag] -> Module () -> Module () -> [Type ()]
wantDerive flag real mine = nub $ map fromTyParens $ wantDeriveFlag flag decls ++ wantDeriveAnnotation real mine
    where decls = filter isDataDecl $ moduleDecls mine


wantDeriveFlag :: [Flag] -> [DataDecl] -> [Type ()]
wantDeriveFlag flags decls = [TyApp () (tyCon x) d | Derive xs <- flags, x <- xs, d <- declst]
    where declst = [tyApps (tyCon $ dataDeclName d) (map tyVar $ dataDeclVars d) | d <- decls]

wantDeriveAnnotation :: Module () -> Module () -> [Type ()]
wantDeriveAnnotation real mine = moduleDerives mine \\ moduleDerives real


moduleDerives :: Module () -> [Type ()]
moduleDerives = concatMap f . moduleDecls
    where
        f (DataDecl _ _ _ (fromDeclHead -> (name, vars)) _ deriv) = g name vars deriv
        f (GDataDecl _ _ _ (fromDeclHead -> (name, vars)) _ _ deriv) = g name vars deriv
        f (DerivDecl _ _ _ (fromIParen -> IRule _ _ _ (fromInstHead -> (name, args)))) = [TyCon () name `tyApps` args]
        f _ = []

        g name vars deriv = [TyCon () a `tyApps` (b:bs) | IRule _ _ _ (fromInstHead -> (a,bs)) <- map fromIParen $ f deriv]
            where b = TyCon () (UnQual () name) `tyApps` map (tyVar . prettyPrint) vars
                  f [Deriving _ _ xs] = xs
                  f _ = []


---------------------------------------------------------------------
-- ACTUALLY DERIVE IT

performDerive :: [Derivation] -> Module () -> [Type ()] -> [String]
performDerive derivations modu = concatMap ((:) "" . f)
    where
        grab = getDecl modu

        g = getDerivation derivations
        f ty = case d ty grab (moduleName modu, grab typ1Name) of
                Left x -> unsafePerformIO $ let res = msg x in hPutStrLn stderr res >> return ["-- " ++ res]
                Right x -> concatMap (lines . prettyPrint) x
            where
                d = derivationOp $ g clsName
                (cls,typ1:_) = fromTyApps ty
                clsName = prettyPrint cls
                typ1Name = tyRoot typ1
                msg x = "Deriving " ++ prettyPrint ty ++ ": " ++ x


getDecl :: Module () -> (String -> Decl ())
getDecl modu = \name -> Map.findWithDefault (error $ "Can't find data type definition for: " ++ name) name mp
    where
        mp = Map.fromList $ concatMap f $ moduleDecls modu
        f x@(DataDecl _ _ _ (fromDeclHead -> (name, _)) _ _) = [(prettyPrint name, x)]
        f x@(GDataDecl _ _ _ (fromDeclHead -> (name, _)) _ _ _) = [(prettyPrint name, x)]
        f x@(TypeDecl _ (fromDeclHead -> (name, _)) _) = [(prettyPrint name, x)]
        f _ = []


getDerivation :: [Derivation] -> String -> Derivation
getDerivation derivations = \name -> Map.findWithDefault (error $ "Don't know how to derive type class: " ++ name) name mp
    where
        mp = Map.fromList $ map (derivationName &&& id) derivations


---------------------------------------------------------------------
-- WRITE IT BACK

writeDerive :: FilePath -> ModuleName () -> [Flag] -> [String] -> IO ()
writeDerive file modu flags xs = do
    -- force the output first, ensure that we don't crash half way through
    () <- length (concat xs) `seq` return ()

    let append = Append `elem` flags
    let output = [x | Output x <- flags]

    let ans = take 1 ["module " ++ x ++ " where" | Modu x <- reverse flags] ++
              ["import " ++ if null i then prettyPrint modu else i | Import i <- flags] ++
              xs

    when append $ do
        src <- readFile' file
        writeGenerated file ans

    forM output $ \o -> writeFile o $ unlines ans
    when (not append && null output) $ putStr $ unlines ans
