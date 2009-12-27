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
import "uniplate" Data.Generics.PlateDirect


-- test tuples
test :: PlateDirect ((), Maybe ()) ()
instance Biplate ((), Maybe ()) () where
    biplate (x1, x2) = plate (,) |* x1 |+ x2

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

test :: PlateDirect Computer Computer
instance Biplate Computer Computer where
    biplate = plateSelf

test :: PlateDirect Computer Double
instance Biplate Computer Double where
    biplate (Laptop x1 x2) = plate Laptop |* x1 |- x2
    biplate x = plate x

test :: PlateDirect (Assoced (Maybe Bool)) Char
instance Biplate (Assoced (Maybe Bool)) Char where
    biplate (Assoced x1 x2) = plate (Assoced x1) ||* x2

-- test following external declarations
test :: PlateDirect (Either Bool Computer) Int
instance Biplate (Either Bool Computer) Int where
    biplate (Right x1) = plate Right |+ x1
    biplate x = plate x

-- test recursive bits
test :: PlateDirect (List Int) Bool
instance Biplate (List Int) Bool where
    biplate x = plate x
-}

import Language.Haskell
import Data.Generics.PlateData
import Data.Derive.Internal.Derivation
import Data.Maybe
import qualified Data.Map as Map
import Control.Arrow
import Control.Monad.State


makePlateDirect :: Derivation
makePlateDirect = derivationParams "PlateDirect" $ \args grab (_,ty) -> simplify $
    let known = map (declName &&& id) knownCtors
        grab2 x = fromMaybe (grab x) $ lookup x known
    in case args of
        _ | not $ null [() | TyVar _ <- universeBi args] -> error "PlateDirect only accepts monomorphic types"
        [] -> make True grab2 x x
            where x = tyApps (tyCon $ dataDeclName ty) $ replicate (dataDeclArity ty) $ TyCon $ Special UnitCon
        [x] -> make True grab2 x x
        [x,y] -> make False grab2 x y
        _ -> error $ "PlateDirect requires exactly one or two arguments, got " ++ show (length args)
        

make :: Bool -> (String -> DataDecl) -> Type -> Type -> Either String [Decl]
make uni grab from to = Right [InstDecl sl [] (UnQual $ Ident $ if uni then "Uniplate" else "Biplate") (from : [to | not uni]) [InsDecl ms]]
    where
        ty = grab $ tyRoot from
        match pat bod = Match sl (Ident $ if uni then "uniplate" else "biplate") [pat] Nothing (UnGuardedRhs bod) (BDecls [])
        ms = if uni || from /= to
             then FunBind $ map (uncurry match) (catMaybes bods) ++ [match (pVar "x") (var "plate" `App` var "x") | any isNothing bods]
             else PatBind sl (pVar "biplate") Nothing (UnGuardedRhs $ var "plateSelf") (BDecls [])
        bods = run (fromTyParens to) $ mapM (make1 grab) $ substData from ty


make1 :: (String -> DataDecl) -> (String,[Type]) -> S (Maybe (Pat, Exp))
make1 grab (name,tys) = do
    ops <- mapM (fmap show . operator grab) tys
    let vars = ['x':show i | i <- [1..length tys]]
        pat = PParen $ PApp (qname name) $ map pVar vars
        (good,bad) = span ((==) "|-" . fst) $ zip ops $ map var vars
        bod = foldl (\x (y,z) -> InfixApp x (QVarOp $ UnQual $ Symbol y) z) (App (var "plate") $ paren $ apps (con name) (map snd good)) bad
    return $ if all (== "|-") ops then Nothing else Just (pat,bod)


data Ans = Hit | Miss | Try | ListHit | ListTry deriving Eq

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


type S a = State (Map.Map Type Ans) a

run :: Type -> S a -> a
run to act = evalState act (Map.singleton to Hit)

operator :: (String -> DataDecl) -> Type -> S Ans
operator grab from = do
    mp <- get
    case Map.lookup from mp of
        Just y -> return y
        Nothing -> do
            fix Miss
    where
        fix ans = do
            s <- get
            modify $ Map.insert from ans
            ans2 <- operator2 grab from
            if ans == ans2
                then return ans
                else put s >> fix ans2


operator2 :: (String -> DataDecl) -> Type -> S Ans
operator2 grab from
    | isTyFun from = return Try
    | Just from2 <- fromTyList from = fmap ansList $ operator grab from2
    | otherwise = case subst from $ grab $ tyRoot from of
        Left from2 -> operator grab from2
        Right ctrs -> fmap ansJoin $ mapM (operator grab) $ concatMap snd ctrs


subst :: Type -> Decl -> Either Type [(String,[Type])]
subst ty x@TypeDecl{} = Left $ substType ty x
subst ty x = Right $ substData ty x

substData :: Type -> Decl -> [(String,[Type])]
substData ty dat = [(ctorDeclName x, map (fromTyParens . transform f . fromBangType . snd) $ ctorDeclFields x) | x <- dataDeclCtors dat]
    where
        rep = zip (dataDeclVars dat) (snd $ fromTyApps $ fromTyParen ty)
        f (TyVar x) = fromMaybe (TyVar x) $ lookup (prettyPrint x) rep
        f x = x

substType :: Type -> Decl -> Type
substType ty (TypeDecl _ _ vars d) = fromTyParens $ transform f d
    where
        rep = zip (map prettyPrint vars) (snd $ fromTyApps ty)
        f (TyVar x) = fromMaybe (TyVar x) $ lookup (prettyPrint x) rep
        f x = x


knownCtors :: [Decl]
knownCtors = map (fromParseResult . parseDecl)
    ["data Int = Int"
    ,"data Bool = Bool"
    ,"data Char = Char"
    ,"data Double = Double"
    ,"data Float = Float"
    ,"data Integer = Integer"
    ,"data Maybe a = Nothing | Just a"
    ,"data Either a b = Left a | Right b"
    ,"type Rational = Ratio Integer"
    ,"data (Integral a) => Ratio a = !a :% !a"
    ,"type String = [Char]"
    ] ++
    listCtor :
    map tupleDefn (0:[2..32])

listCtor = DataDecl sl  DataType [] (Ident "[]") [UnkindedVar $ Ident "a"]
    [QualConDecl sl [] [] $ ConDecl (Ident "[]") []
    ,QualConDecl sl [] [] $ ConDecl (Ident "(:)") [UnBangedTy $ tyVar "a", UnBangedTy $ TyList $ tyVar "a"]] []

tupleDefn :: Int -> Decl
tupleDefn n = DataDecl sl DataType [] (Ident s) (map (UnkindedVar . Ident) vars) [QualConDecl sl [] [] $ ConDecl (Ident s) (map (UnBangedTy . tyVar) vars)] []
    where s = "(" ++ replicate (n - 1) ',' ++ ")"
          vars = ['v':show i | i <- [1..n]]
