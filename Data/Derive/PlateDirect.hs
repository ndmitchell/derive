{-# LANGUAGE PatternGuards #-}
-- NOTE: Cannot be guessed as it relies on type information

-- | Derive Uniplate and Biplate using the Direct combinators.
--   You must specific monomorphic instances, i.e:
--
-- > data Foo a = Foo a Int
-- >      deriving ({-! PlateDirect (Foo Int), PlateDirect (Foo Int) Int !-})
--
--   All types referred to must be in scope at the time.
--
--   @deriving PlateDirect@ with no arguments will be assumed to derive the Uniplate
--   instance on all types being unit.
module Data.Derive.PlateDirect(makePlateDirect) where


{-
test :: PlateDirect (Sample Int)

instance Uniplate (Sample Int) where
    uniplate x = plate x


test :: PlateDirect (Sample Int) Int

instance Biplate (Sample Int) Int where
    biplate (Second x1 x2) = plate Second |* x1 |* x2
    biplate (Third x1) = plate Third |* x1
    biplate x = plate x

test :: PlateDirect Computer

instance Uniplate Computer where
    uniplate x = plate x

test :: PlateDirect Computer Double

instance Biplate Computer Double where
    biplate (Laptop x1 x2) = plate Laptop |* x1 |- x2
    biplate x = plate x

test :: PlateDirect (Assoced (Maybe Bool)) Char

instance Biplate (Assoced (Maybe Bool)) Char where
    biplate (Assoced x1 x2) = plate (Assoced x1) ||* x2
-}

import Language.Haskell
import Data.Generics.PlateData
import Data.Derive.Internal.Derivation
import Data.Maybe
import Control.Arrow


makePlateDirect :: Derivation
makePlateDirect = derivationParams "PlateDirect" $ \args grab (_,ty) ->
    let known = map (declName &&& id) knownCtors
        grab2 x | declName ty == x = ty
        grab2 x = fromMaybe (grab x) $ lookup x known
    in case args of
        _ | not $ null [() | TyVar _ <- universeBi args] -> error "PlateDirect only accepts monomorphic types"
        [] -> make True grab2 x ty x
            where x = tyApps (tyCon $ dataDeclName ty) $ replicate (dataDeclArity ty) $ TyCon $ Special UnitCon
        [x] -> make True grab2 x ty x
        [x,y] -> make False grab2 y ty x
        _ -> error $ "PlateDirect requires exactly one or two arguments, got " ++ show (length args)
        

make :: Bool -> (String -> DataDecl) -> Type -> DataDecl -> Type -> Either String [Decl]
make uni grab to ty from = Right [InstDecl sl [] (UnQual $ Ident $ if uni then "Uniplate" else "Biplate") (from : [to | not uni]) [InsDecl $ FunBind ms]]
    where
        match pat bod = Match sl (Ident $ if uni then "uniplate" else "biplate") [pat] Nothing (UnGuardedRhs bod) (BDecls [])
        ms = map (uncurry match) (catMaybes bods) ++ [match (pVar "x") (var "plate" `App` var "x") | any isNothing bods]
        bods = map (make1 grab to) $ substData from ty


make1 :: (String -> DataDecl) -> Type -> (String,[Type]) -> Maybe (Pat, Exp)
make1 grab to (name,tys)
        | all (== "|-") ops = Nothing
        | otherwise = Just (pat,bod)
    where
        ops = map (show . operator grab to) tys
        vars = ['x':show i | i <- [1..length tys]]
        pat = PParen $ PApp (qname name) $ map pVar vars
        bod = foldl (\x (y,z) -> InfixApp x (QVarOp $ UnQual $ Symbol y) z) (App (var "plate") $ paren $ apps (con name) (map snd good)) bad
            where (good,bad) = span ((==) "|-" . fst) $ zip ops $ map var vars


data Ans = Hit | Miss | Try | ListHit | ListTry

instance Show Ans where
    show Hit = "|*"
    show Miss = "|-"
    show Try = "|+"
    show ListHit = "||*"
    show ListTry = "||+"

ansList Hit = ListHit
ansList Miss = Miss
ansList _ = ListTry


ansJoin (Miss:xs) = ansJoin xs
ansJoin [] = Miss
ansJoin _ = Try


operator :: (String -> DataDecl) -> Type -> Type -> Ans
operator grab to from
    | isTyParen to || isTyParen from = operator grab (fromTyParen to) (fromTyParen from)
    | to == from = Hit
    | Just from2 <- fromTyList from = ansList $ operator grab to from2
    | otherwise = case subst from $ grab $ prettyPrint $ fst $ fromTyApps from of
        Left from2 -> operator grab to from2
        Right ctrs -> ansJoin $ map (operator grab to) $ concatMap snd ctrs


subst :: Type -> Decl -> Either Type [(String,[Type])]
subst ty x@TypeDecl{} = Left $ substType ty x
subst ty x = Right $ substData ty x

substData :: Type -> Decl -> [(String,[Type])]
substData ty dat = [(ctorDeclName x, map (transform f . fromBangType . snd) $ ctorDeclFields x) | x <- dataDeclCtors dat]
    where
        rep = zip (dataDeclVars dat) (snd $ fromTyApps $ fromTyParen ty)
        f (TyVar x) = fromMaybe (TyVar x) $ lookup (prettyPrint x) rep
        f x = x

substType :: Type -> Decl -> Type
substType ty (TypeDecl _ _ vars d) = transform f d
    where
        rep = zip (map prettyPrint vars) (snd $ fromTyApps $ fromTyParen ty)
        f (TyVar x) = fromMaybe (TyVar x) $ lookup (prettyPrint x) rep
        f x = x


knownCtors :: [Decl]
knownCtors = map (fromParseResult . parseDecl)
    ["data Int = Int"
    ,"data Bool = Bool"
    ,"data Char = Char"
    ,"data Double = Double"
    ,"data Float = Float"
    ,"data Maybe a = Nothing | Just a"
    ,"type String = [Char]"
    ]
