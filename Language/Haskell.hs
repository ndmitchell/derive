
module Language.Haskell(module Language.Haskell, module Language.Haskell.Exts) where

import Language.Haskell.Exts hiding (var,app,binds,paren)
import Data.List
import Data.Generics.PlateData
import Data.Data
import Data.Char


infix 1 ?
True ? b = const b
False ? b = id



-- insert explicit foralls
foralls :: Type -> Type
foralls x = TyForall (Just $ map UnkindedVar $ nub [y | TyVar y <- universe x]) [] x


tyApps x [] = x
tyApps x (y:ys) = tyApps (TyApp x y) ys

isTyFun TyFun{} = True
isTyFun _ = False

x ~= y = prettyPrint x == y


simplify :: Data a => a -> a
simplify = transformBi fDecl . transformBi fMatch . transformBi fPat . transformBi fTyp . transformBi fExp
    where
        fExp (App op (List xs))
            | op ~= "length" = Lit $ Int $ fromIntegral $ length xs
            | op ~= "head" = head xs
        fExp (InfixApp (Lit (Int i)) op (Lit (Int j)))
            | op ~= "-" = Lit $ Int $ i - j
            | op ~= "+" = Lit $ Int $ i + j
            | op ~= ">" = Con $ UnQual $ Ident $ show $ i > j
        fExp (InfixApp x op y)
            | op ~= "`const`" = x
            | op ~= "&&" && y ~= "True" = x
        fExp (App (App con x) y) | con ~= "const" = x
        fExp (App choose (Tuple [x@(ExpTypeSig _ y _),z])) | choose ~= "choose" && y == z = App (var "return") x
        fExp (App op x) | op ~= "id" = x
        fExp (InfixApp (App when true) dot res)
            | when ~= "when" && true ~= "True" = res
        fExp (App (LeftSection x op) y) = InfixApp x op (paren y)
        fExp (Paren x) | isAtom x = x
        fExp (Do [Qualifier x]) = x
        fExp (Do (Qualifier (App ret unit):xs)) | ret ~= "return" && unit ~= "()" = fExp $ Do xs
        fExp (Do (Generator _ (PVar x) (App ret y):xs)) | ret ~= "return" && once x2 xs = simplify $ Do $ subst x2 y xs
            where x2 = Var $ UnQual x
        fExp (Case (ExpTypeSig _ x@Lit{} _) alts) = fExp $ Case x alts
        fExp (Case (Lit x) alts) | good /= [] = head good
            where good = [z | Alt _ (PLit y) (UnGuardedAlt z) (BDecls []) <- alts, y == x]
        fExp (If x t f)
            | x ~= "True" = t
            | x ~= "False" = f
        fExp (App (App when b) x)
            | b ~= "True" = x
            | b ~= "False" = App (Var $ UnQual $ Ident "return") (Con $ Special $ TupleCon Boxed 0)
        fExp (App (Paren (Lambda _ [PVar x] y)) z) | once x2 y = fExp $ subst x2 z y
            where x2 = Var $ UnQual x
        fExp x = x

        fTyp (TyApp x y) | x ~= "[]" = TyApp (TyCon (Special ListCon)) y
        fTyp (TyParen x@(TyApp (TyCon (Special ListCon)) _)) = x
        fTyp (TyParen x@TyCon{}) = x
        fTyp (TyParen x@TyVar{}) = x
        fTyp x = x

        fPat (PParen x@(PApp _ [])) = x
        fPat x = x

        fMatch (Match sl nam pat sig (GuardedRhss [GuardedRhs _ [Qualifier x] bod]) decls)
            | x ~= "True" = fMatch $ Match sl nam pat sig (UnGuardedRhs bod) decls
        fMatch (Match sl nam [PVar x] sig (UnGuardedRhs (Case (Var (UnQual x2)) [Alt _ pat (UnGuardedAlt y) (BDecls [])])) decls)
            | x == x2 = fMatch $ Match sl nam [PParen pat] sig (UnGuardedRhs y) decls
        fMatch (Match a b c d e bind) = fBinds (Match a b c d e) bind

        fDecl (PatBind a b c d bind) = fBinds (PatBind a b c d) bind
        fDecl (FunBind xs) = FunBind $ filter (not . isGuardFalse) xs
        fDecl x = x

        fBinds context (BDecls bind) | inline /= [] =
                simplify $ subst (Var $ UnQual from) to $ context $ BDecls $ take i bind ++ drop (i+1) bind
            where
                f (PatBind _ (PVar x) Nothing (UnGuardedRhs bod) (BDecls [])) = [(x,bod)]
                f (FunBind [Match _ x [PVar v] Nothing (UnGuardedRhs (Paren (App bod (Var v2)))) (BDecls [])])
                    | UnQual v == v2 = [(x,bod)]
                f (FunBind [Match sl x pat Nothing (UnGuardedRhs bod) (BDecls [])]) = [(x,Paren $ Lambda sl pat bod)]
                f _ = []

                (i,from,to) = head inline
                inline = [(i, x, bod)
                         | (i,b) <- zip [0..] bind, (x,bod) <- f b
                         , isAtom bod || once (Var $ UnQual x) (context $ BDecls bind)]
        fBinds a y = a y

        subst from to = transformBi $ \x -> if x == from then to else x
        once x y = length (filter (== x) (universeBi y)) <= 1  

isGuardFalse (Match sl nam pat sig (GuardedRhss [GuardedRhs _ [Qualifier x] bod]) decls) = x ~= "False"
isGuardFalse _ = False


isAtom Con{} = True
isAtom Var{} = True
isAtom Lit{} = True
isAtom _ = False


paren x = if isAtom x then x else Paren x

sl = SrcLoc "" 0 0

noSl mr = transformBi (const sl) mr


title (x:xs) = toUpper x : xs

qname = UnQual . name
var = Var . qname
con = Con . qname
tyVar = TyVar . name
tyVarBind = UnkindedVar . name
tyCon = TyCon . qname
pVar = PVar . name
qvop = QVarOp . UnQual . Symbol

dataDeclType :: DataDecl -> Type
dataDeclType d = tyApp (tyCon $ dataDeclName d) (map tyVar $ dataDeclVars d)

dataDeclFields :: DataDecl -> [String]
dataDeclFields = sort . nub . filter (not . null) . map fst . concatMap ctorDeclFields . dataDeclCtors


-- A declaration that is either a DataDecl of GDataDecl
type DataDecl = Decl
type CtorDecl = Either QualConDecl GadtDecl
type FieldDecl = [(String, BangType)]

type FullDataDecl = (ModuleName, DataDecl)


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


apps x [] = x
apps x (y:ys) = apps (App x y) ys


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

dataDeclArity :: DataDecl -> Int
dataDeclArity = length . dataDeclVars

dataDeclCtors :: DataDecl -> [CtorDecl]
dataDeclCtors (DataDecl _ _ _ _ _ ctors _) = map Left ctors


ctorDeclName :: CtorDecl -> String
ctorDeclName (Left (QualConDecl _ _ _ (ConDecl name _))) = prettyPrint name
ctorDeclName (Left (QualConDecl _ _ _ (RecDecl name _))) = prettyPrint name

ctorDeclFields :: CtorDecl -> FieldDecl
ctorDeclFields (Left (QualConDecl _ _ _ (ConDecl name fields))) = map ((,) "") fields
ctorDeclFields (Left (QualConDecl _ _ _ (RecDecl name fields))) = [(prettyPrint a, b) | (as,b) <- fields, a <- as]

ctorDeclArity :: CtorDecl -> Int
ctorDeclArity = length . ctorDeclFields
