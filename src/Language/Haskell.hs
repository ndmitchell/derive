{-# LANGUAGE PatternGuards #-}
module Language.Haskell(module Language.Haskell, module Language.Haskell.Exts) where

import Language.Haskell.Exts hiding (var,app,binds,paren,FieldDecl)
import qualified Language.Haskell.Exts as HSE
import Data.List
import Data.Generics.Uniplate.Data
import Data.Data
import Data.Char
import Data.Maybe
import Control.Arrow


infix 1 ?
True ? b = const b
False ? b = id



-- insert explicit foralls
foralls :: Type () -> Type ()
foralls x = TyForall () (Just $ map (UnkindedVar ()) $ nub [y | TyVar _ y <- universe x]) Nothing x


tyApps x [] = x
tyApps x (y:ys) = tyApps (TyApp () x y) ys


fromTyApps (TyTuple _ _ xs) = (tyCon $ "(" ++ replicate (length xs - 1) ',' ++ ")", xs)
fromTyApps (TyApp _ x y) = let (a,b) = fromTyApps x in (a, b ++ [y])
fromTyApps (TyList _ x) = (TyCon () $ Special () $ ListCon (), [x])
fromTyApps x = (x, [])

fromTyTuple (TyTuple _ _ xs) = xs
fromTyTuple x = [x]

fromTyParen (TyParen () x) = fromTyParen x
fromTyParen x = x

fromTyParens = transform fromTyParen

tyRoot = prettyPrint . fst . fromTyApps . fromTyParen

isTyFun :: Type () -> Bool
isTyFun TyFun{} = True
isTyFun _ = False

isTyParen TyParen{} = True ; isTyParen _ = False

fromTyList (TyList _ x) = Just x
fromTyList (TyApp _ (TyCon _ (Special _ ListCon{})) x) = Just x
fromTyList x = Nothing


x ~= y = prettyPrint x == y


appP x@App{} y = App () x y
appP x y = App () (paren x) (paren y)


simplify :: Data a => a -> a
simplify = transformBi fDecl . transformBi fMatch . transformBi fPat . transformBi fTyp . transformBi fExp
    where
        fExp :: Exp () -> Exp ()
        fExp (App _ op (List _ xs))
            | op ~= "length" = Lit () $ Int () (fromIntegral $ length xs) (show $ length xs)
            | op ~= "head" = head xs
            | op ~= "null" = con $ show $ null xs
        fExp (InfixApp _ (Lit _ (Int _ i _)) op (Lit _ (Int _ j _)))
            | op ~= "-" = Lit () $ Int () (i - j) (show $ i-j)
            | op ~= "+" = Lit () $ Int () (i + j) (show $ i+j)
            | op ~= ">" = Con () $ UnQual () $ Ident () $ show $ i > j
        fExp (InfixApp _ x op y)
            | op ~= "`const`" = x
            | op ~= "&&" && y ~= "True" = x
            | x ~= "id" && op ~= "." = y
            | y ~= "id" && op ~= "." = x
        fExp (InfixApp _ (Lit _ (String _ x _)) op (Lit _ (String _ y _))) | op ~= "++" = Lit () $ String () (x ++ y) (show $ x ++ y)
        fExp (App _ (App _ (App _ flp f) x) y) | flp ~= "flip" = fExp $ appP (fExp $ appP f y) x
        fExp (App _ (Paren _ x@App{}) y) = fExp $ App () x y
        fExp (App _ (Paren _ (InfixApp _ x op y)) z) | op ~= "." = fExp $ appP x $ fExp $ appP y z
        fExp (App _ op x) | op ~= "id" = x
        fExp (App _ (App _ flp con) x) | flp ~= "flip" && con ~= "const" = var "id"
        fExp (App _ (App _ con x) y) | con ~= "const" = x
        fExp (App _ choose (Tuple _ _ [x@(ExpTypeSig _ y _),z])) | choose ~= "choose" && y == z = fExp $ App () (var "return") x
        fExp (App _ op x) | op ~= "id" = x
        fExp (InfixApp _ (App _ when true) dot res)
            | when ~= "when" && true ~= "True" = res
        fExp (InfixApp _ x y z) | y ~= "++" && z ~= "[]" = x
        fExp (App _ (LeftSection _ x op) y) = fExp $ InfixApp () x op (paren y)
        fExp (Paren _ x) | isAtom x = x
        fExp (Do _ [Qualifier _ x]) = x
        fExp (Do _ (Qualifier _ (App _ ret unit):xs)) | ret ~= "return" && unit ~= "()" = fExp $ Do () xs
        fExp (Do _ (Generator _ (PVar _ x) (App _ ret y):xs)) | ret ~= "return" && once x2 xs = simplify $ Do () $ subst x2 y xs
            where x2 = Var () $ UnQual () x
        fExp (Case _ (ExpTypeSig _ x@Lit{} _) alts) = fExp $ Case () x alts
        fExp (Case _ (Lit _ x) alts) | good:_ <- good = good
            where good = [z | Alt _ (PLit _ Signless{} y) (UnGuardedRhs _ z) Nothing <- alts, y == x]
        fExp (If _ x t f)
            | x ~= "True" = t
            | x ~= "False" = f
        fExp (App _ (App _ when b) x)
            | when ~= "when" && b ~= "True" = x
            | when ~= "when" && b ~= "False" = App () (Var () $ UnQual () $ Ident () "return") (Con () $ Special () $ TupleCon () Boxed 0)
        fExp (App _ (Paren _ (Lambda _ [PVar _ x] y)) z) | once x2 y = fExp $ subst x2 z y
            where x2 = Var () $ UnQual () x
        fExp (App _ (Paren _ (Lambda _ [PWildCard _] x)) _) = x
        fExp (Lambda s ps x) = Lambda s (minPat x ps) x
        fExp (Con _ x) = Con () $ rename x
        fExp x = x

        fTyp :: Type () -> Type ()
        fTyp (TyApp _ x y) | x ~= "[]" = TyList () y
        fTyp (TyApp _ (TyCon _ (Special _ ListCon{})) x) = TyList () x
        fTyp (TyParen _ x@TyCon{}) = x
        fTyp (TyParen _ x@TyVar{}) = x
        fTyp (TyParen _ x@TyList{}) = x
        fTyp (TyCon _ nam) = TyCon () $ rename nam
        fTyp x = x

        fPat :: Pat () -> Pat ()
        fPat (PParen _ x@(PApp _ _ [])) = x
        fPat (PParen _ (PParen _ x)) = PParen () x
        fPat (PApp _ nam xs) = case rename nam of
            Special _ (TupleCon _ Boxed _) -> PTuple () Boxed xs
            nam -> PApp () nam xs
        fPat (PParen _ (PTuple _ l xs)) = PTuple () l xs
        fPat x = x

        fMatch :: Match () -> Match ()
        fMatch (Match sl nam pat (GuardedRhss _ [GuardedRhs _ [Qualifier _ x] bod]) decls)
            | x ~= "True" = fMatch $ Match sl nam pat (UnGuardedRhs () bod) decls
        fMatch (Match sl nam [PVar _ x] (UnGuardedRhs _ (Case _ (Var _ (UnQual _ x2)) [Alt _ pat (UnGuardedRhs _ y) Nothing])) decls)
            | x == x2 = fMatch $ Match sl nam [PParen () pat] (UnGuardedRhs () y) decls
        fMatch o@(Match a b c d bind) = fBinds (Match a b (minPat o c) d) bind

        fDecl :: Decl () -> Decl ()
        fDecl (PatBind a b c bind) = fBinds (PatBind a b c) bind
        fDecl (FunBind _ xs) = FunBind () $ filter (not . isGuardFalse) xs
        fDecl x = x

        fBinds context Nothing = context Nothing
        fBinds context (Just (BDecls _ bind)) | inline /= [] =
                simplify $ subst (Var () $ UnQual () from) to $ context $
                    let xs = take i bind ++ drop (i+1) bind in if null xs then Nothing else Just $ BDecls () xs
            where
                f (PatBind _ (PVar _ x) (UnGuardedRhs _ bod) Nothing) = [(x,bod)]
                f (FunBind _ [Match _ x [PVar _ v] (UnGuardedRhs _ (Paren _ (App _ bod (Var _ v2)))) Nothing])
                    | UnQual () v == v2 = [(x,bod)]
                f (FunBind _ [Match sl x pat (UnGuardedRhs _ bod) Nothing]) = [(x,Paren () $ Lambda sl pat bod)]
                f _ = []

                (i,from,to) = head inline
                inline = [(i, x, bod)
                         | (i,b) <- zip [0..] bind, (x,bod) <- f b
                         , isAtom bod || once (Var () $ UnQual () x) (context $ Just $ BDecls () bind)]
        fBinds a y = a y

        subst from to = transformBi $ \x -> if x == from then to else x
        once x y = length (filter (== x) (universeBi y)) <= 1

        minPat o ps = transformBi f ps
            where
                known = nub [x | UnQual _ x <- universeBi o]
                f (PVar () x) | x `notElem` known = PWildCard ()
                f (PAsPat () x y) | x `notElem` known = y
                f x = x



isGuardFalse (Match sl nam pat (GuardedRhss _ [GuardedRhs _ [Qualifier _ x] bod]) decls) = x ~= "False"
isGuardFalse _ = False


rename (UnQual _ (Ident _ ('(':xs@(x:_))))
    | x == ',' = Special () $ TupleCon () Boxed $ length xs
    | x /= ')' = UnQual () $ Symbol () $ init xs
rename x = x


isAtom Con{} = True
isAtom Var{} = True
isAtom Lit{} = True
isAtom Paren{} = True
isAtom _ = False


paren x = if isAtom x then x else Paren () x

sl = SrcLoc "" 0 0

noSl mr = transformBi (const sl) mr


isIdent (x:xs) = isAlpha x || x == '_'
title (x:xs) = toUpper x : xs

qname = UnQual () . name
var = Var () . qname
con = Con () . qname
tyVar = TyVar () . name
tyVarBind = UnkindedVar () . name
tyCon = TyCon () . qname
pVar = PVar () . name
qvop = QVarOp () . UnQual () . Symbol ()

dataDeclType :: DataDecl -> Type ()
dataDeclType d = tyApp (tyCon $ dataDeclName d) (map tyVar $ dataDeclVars d)

dataDeclFields :: DataDecl -> [String]
dataDeclFields = sort . nub . filter (not . null) . map fst . concatMap ctorDeclFields . dataDeclCtors


-- A declaration that is either a DataDecl of GDataDecl
type DataDecl = Decl ()
type CtorDecl = Either (QualConDecl ()) (GadtDecl ())
type FieldDecl = [(String, Type ())]

type FullDataDecl = (ModuleName (), DataDecl)


moduleName (Module _ (Just (ModuleHead _ name _ _)) _ _ _) = name
moduleDecls (Module _ _ _ _ decls) = decls
moduleImports (Module _ _ _ imps _) = imps
modulePragmas (Module _ _ pragmas _ _) = pragmas


showDecls x = unlines $ map prettyPrint x


tyApp x [] = x
tyApp x xs = TyApp () (tyApp x $ init xs) (last xs)


tyFun [x] = x
tyFun (x:xs) = TyFun () x (tyFun xs)


apps x [] = x
apps x (y:ys) = apps (App () x y) ys


bind :: String -> [Pat ()] -> Exp () -> Decl ()
bind s p e = binds s [(p,e)]

binds :: String -> [([Pat ()], Exp ())] -> Decl ()
binds n [([],e)] = PatBind () (pVar n) (UnGuardedRhs () e) Nothing
binds n xs = FunBind () [Match () (name n) p (UnGuardedRhs () e) Nothing | (p,e) <- xs]


isDataDecl :: Decl () -> Bool
isDataDecl DataDecl{} = True
isDataDecl GDataDecl{} = True
isDataDecl _ = False

dataDeclName :: DataDecl -> String
dataDeclName (DataDecl _ _ _ name _ _) = prettyPrint $ fst $ fromDeclHead name
dataDeclName (GDataDecl _ _ _ name _ _ _) = prettyPrint $ fst $ fromDeclHead name

fromDeclHead :: DeclHead a -> (Name a, [TyVarBind a])
fromDeclHead (DHead _ n) = (n, [])
fromDeclHead (DHInfix _ x n) = (n, [x])
fromDeclHead (DHParen _ x) = fromDeclHead x
fromDeclHead (DHApp _ dh x) = second (++[x]) $ fromDeclHead dh

fromIParen :: InstRule a -> InstRule a
fromIParen (IParen _ x) = fromIParen x
fromIParen x = x

fromInstHead :: InstHead a -> (QName a, [Type a])
fromInstHead (IHCon _ x) = (x, [])
fromInstHead (IHInfix _ t x) = (x, [t])
fromInstHead (IHParen _ x) = fromInstHead x
fromInstHead (IHApp l hd t) = second (++ [t]) $ fromInstHead hd


dataDeclVars :: DataDecl -> [String]
dataDeclVars (DataDecl _ _ _ hd _ _) = map f $ snd $ fromDeclHead hd
    where f (KindedVar _ x _) = prettyPrint x
          f (UnkindedVar _ x) = prettyPrint x

dataDeclVarsStar :: DataDecl -> [String]
dataDeclVarsStar (DataDecl _ _ _ hd _ _) = mapMaybe f $ snd $ fromDeclHead hd
    where f (UnkindedVar _ x) = Just $ prettyPrint x
          f (KindedVar _ x (KindStar _)) = Just $ prettyPrint x
          f _ = Nothing

dataDeclArity :: DataDecl -> Int
dataDeclArity = length . dataDeclVars

dataDeclCtors :: DataDecl -> [CtorDecl]
dataDeclCtors (DataDecl _ _ _ _ ctors _) = map Left ctors


ctorDeclName :: CtorDecl -> String
ctorDeclName = prettyPrint . ctorDeclName'

ctorDeclName' :: CtorDecl -> Name ()
ctorDeclName' (Left (QualConDecl _ _ _ (ConDecl _ name _))) = name
ctorDeclName' (Left (QualConDecl _ _ _ (InfixConDecl _ _ name _))) = name
ctorDeclName' (Left (QualConDecl _ _ _ (RecDecl _ name _))) = name

ctorDeclFields :: CtorDecl -> FieldDecl
ctorDeclFields (Left (QualConDecl _ _ _ (ConDecl _ name fields))) = map ((,) "") fields
ctorDeclFields (Left (QualConDecl _ _ _ (InfixConDecl _ x1 name x2))) = map ((,) "") [x1,x2]
ctorDeclFields (Left (QualConDecl _ _ _ (RecDecl _ name fields))) = [(prettyPrint a, b) | HSE.FieldDecl _ as b <- fields, a <- as]

ctorDeclArity :: CtorDecl -> Int
ctorDeclArity = length . ctorDeclFields

declName :: Decl () -> String
declName (DataDecl _ _ _ name _ _) = prettyPrint $ fst $ fromDeclHead name
declName (GDataDecl _ _ _ name _ _ _) = prettyPrint $ fst $ fromDeclHead name
declName (TypeDecl _ name _) = prettyPrint $ fst $ fromDeclHead name
