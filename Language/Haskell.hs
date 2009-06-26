
module Language.Haskell(module Language.Haskell, module Language.Haskell.Exts) where

import Language.Haskell.Exts hiding (var,app,binds)
import Data.List
import Data.Generics.PlateData
import Data.Char


infix 1 ?
True ? b = const b
False ? b = id


x ~= y = prettyPrint x == y


simplify :: [Decl] -> [Decl]
simplify = transformBi fPat . transformBi fTyp . transformBi fExp
    where
        fExp (App op (List xs))
            | op ~= "length" = Lit $ Int $ fromIntegral $ length xs
            | op ~= "head" = head xs
        fExp (InfixApp (Lit (Int i)) op (Lit (Int j)))
            | op ~= "-" = Lit $ Int $ i - j
            | op ~= "+" = Lit $ Int $ i + j
            | op ~= ">" = Con $ UnQual $ Ident $ show $ i > j
        fExp (InfixApp x op y) | op ~= "`const`" = x
        fExp (App (App con x) y) | con ~= "const" = x
        fExp (Paren (Var x)) = Var x
        fExp (Paren (Lit x)) = Lit x
        fExp x = x

        fTyp (TyApp x y) | x ~= "[]" = TyApp (TyCon (Special ListCon)) y
        fTyp (TyParen x@(TyApp (TyCon (Special ListCon)) _)) = x
        fTyp (TyParen x@TyCon{}) = x
        fTyp (TyParen x@TyVar{}) = x
        fTyp x = x

        fPat (PParen x@(PApp _ [])) = x
        fPat x = x


sl = SrcLoc "" 0 0

noSl mr = transformBi (const sl) mr


title (x:xs) = toUpper x : xs

qname = UnQual . name
var = Var . qname
con = Con . qname
tyVar = TyVar . name
tyCon = TyCon . qname
pVar = PVar . name


dataDeclType :: DataDecl -> Type
dataDeclType d = tyApp (tyCon $ dataDeclName d) (map tyVar $ dataDeclVars d)

dataDeclFields :: DataDecl -> [String]
dataDeclFields = sort . nub . filter (not . null) . map fst . concatMap ctorDeclFields . dataDeclCtors


-- A declaration that is either a DataDecl of GDataDecl
type DataDecl = Decl
type CtorDecl = Either QualConDecl GadtDecl
type FieldDecl = [(String, BangType)]

type FullDataDecl = (ModuleName, Decl)


moduleName (Module _ name _ _ _ _ _) = name
moduleDecls (Module _ _ _ _ _ _ decls) = decls
moduleImports (Module _ _ _ _ _ imps _) = imps
modulePragmas (Module _ _ pragmas _ _ _ _) = pragmas


showDecls x = unlines $ map prettyPrint x


unParseOk (ParseOk x) = x


tyApp x [] = x
tyApp x xs = TyApp (tyApp x $ init xs) (last xs)


tyFun [x] = x
tyFun (x:xs) = TyFun x (tyFun xs)


app x [] = x
app x xs = app (App x (head xs)) (tail xs)


bind :: String -> [Pat] -> Exp -> Decl
bind s p e = binds s [(p,e)]

binds :: String -> [([Pat], Exp)] -> Decl
binds n [([],e)] = PatBind sl (pVar n) Nothing (UnGuardedRhs e) (BDecls [])
binds n xs = FunBind [Match sl (name n) p Nothing (UnGuardedRhs e) (BDecls []) | (p,e) <- xs]


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
