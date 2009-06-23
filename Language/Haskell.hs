
module Language.Haskell(module Language.Haskell, module Language.Haskell.Exts) where

import Language.Haskell.Exts
import Data.List


x ~= y = prettyPrint x == y
    
sl = SrcLoc "" 0 0

-- A declaration that is either a DataDecl of GDataDecl
type DataDecl = Decl
type CtorDecl = Either QualConDecl GadtDecl
type FieldDecl = [(String, BangType)]

type FullDataDecl = (ModuleName, Decl)

isUnknownDeclPragma UnknownDeclPragma{} = True
isUnknownDeclPragma _ = False


moduleName (Module _ name _ _ _ _ _) = name
moduleDecls (Module _ _ _ _ _ _ decls) = decls
moduleImports (Module _ _ _ _ _ imps _) = imps
modulePragmas (Module _ _ pragmas _ _ _ _) = pragmas


showDecls x = unlines $ map prettyPrint x


unParseOk (ParseOk x) = x


tyApp x [] = x
tyApp x xs = TyApp (tyApp x $ init xs) (last xs)


fromBangType (BangedTy x) = x
fromBangType (UnBangedTy x) = x
fromBangType (UnpackedTy x) = x


isDataDecl :: Decl -> Bool
isDataDecl DataDecl{} = True
isDataDecl GDataDecl{} = True
isDataDecl _ = False


dataDeclSrcLoc :: DataDecl -> SrcLoc
dataDeclSrcLoc (DataDecl sl _ _ _ _ _ _) = sl
dataDeclSrcLoc (GDataDecl sl _ _ _ _ _ _ _) = sl

dataDeclName :: DataDecl -> String
dataDeclName (DataDecl _ _ _ name _ _ _) = prettyPrint name
dataDeclName (GDataDecl _ _ _ name _ _ _ _) = prettyPrint name

dataDeclVars :: DataDecl -> [String]
dataDeclVars (DataDecl _ _ _ _ vars _ _) = map prettyPrint vars

dataDeclCtors :: DataDecl -> [CtorDecl]
dataDeclCtors (DataDecl _ _ _ _ _ ctors _) = map Left ctors


ctorDeclName :: CtorDecl -> String
ctorDeclName (Left (QualConDecl _ _ _ (ConDecl name _))) = prettyPrint name
ctorDeclName (Left (QualConDecl _ _ _ (RecDecl name _))) = prettyPrint name

ctorDeclFields :: CtorDecl -> FieldDecl
ctorDeclFields (Left (QualConDecl _ _ _ (ConDecl name fields))) = map ((,) "") fields
ctorDeclFields (Left (QualConDecl _ _ _ (RecDecl name fields))) = [(prettyPrint a, b) | (as,b) <- fields, a <- as]
