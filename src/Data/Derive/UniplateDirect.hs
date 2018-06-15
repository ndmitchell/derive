{-# LANGUAGE PatternGuards #-}
-- NOTE: Cannot be guessed as it relies on type information

-- | Derive Uniplate and Biplate using the Direct combinators.
--   You must request monomorphic instances, i.e:
--
-- > data Foo a = Foo a (Bool, a)
-- >
-- > {-!
-- > deriving instance UniplateDirect (Foo Int)
-- > deriving instance UniplateDirect (Bool, Int) Int
-- > deriving instance UniplateDirect (Foo Int) Int
-- > !-}
--
--   This will generate the instances @Uniplate (Foo Int)@,
--   @Biplate (Bool, Int) Int@ and @Biplate (Foo Int) Int@.
--   Generally, start with the instance you are after (e.g. @UniplateDirect (Foo Int)@),
--   try to compile and add further instances as necessary. @UniplateDirect@ with
--   one argument derives Uniplate, and with two arguments derives Biplate.
--
--   @deriving UniplateDirect@ on a data type with no arguments derives Uniplate
--   with all type parameters defaulting to @()@.
module Data.Derive.UniplateDirect(makeUniplateDirect) where


{-
import "uniplate" Data.Generics.Uniplate.Direct


-- test tuples
test :: UniplateDirect ((), Maybe ()) ()
instance Biplate ((), Maybe ()) () where
    {-# INLINE biplate #-}
    biplate (x1, x2) = plate (,) |* x1 |+ x2

test :: UniplateDirect (Sample Int)
instance Uniplate (Sample Int) where
    {-# INLINE uniplate #-}
    uniplate x = plate x

test :: UniplateDirect (Sample Int) Int
instance Biplate (Sample Int) Int where
    {-# INLINE biplate #-}
    biplate (Second x1 x2) = plate Second |* x1 |* x2
    biplate (Third x1) = plate Third |* x1
    biplate x = plate x

test :: UniplateDirect Computer
instance Uniplate Computer where
    {-# INLINE uniplate #-}
    uniplate x = plate x

test :: UniplateDirect Computer Computer
instance Biplate Computer Computer where
    {-# INLINE biplate #-}
    biplate = plateSelf

test :: UniplateDirect Computer Double
instance Biplate Computer Double where
    {-# INLINE biplate #-}
    biplate (Laptop x1 x2) = plate Laptop |* x1 |- x2
    biplate x = plate x

test :: UniplateDirect (Assoced (Maybe Bool)) Char
instance Biplate (Assoced (Maybe Bool)) Char where
    {-# INLINE biplate #-}
    biplate (Assoced x1 x2) = plate (Assoced x1) ||* x2

-- test following external declarations
test :: UniplateDirect (Either Bool Computer) Int
instance Biplate (Either Bool Computer) Int where
    {-# INLINE biplate #-}
    biplate (Right x1) = plate Right |+ x1
    biplate x = plate x

-- test recursive bits
test :: UniplateDirect (List Int) Bool
instance Biplate (List Int) Bool where
    {-# INLINE biplate #-}
    biplate x = plate x
-}

import Language.Haskell
import Data.Generics.Uniplate.DataOnly
import Data.Derive.Internal.Derivation
import Data.Maybe
import qualified Data.Map as Map
import Control.Arrow
import Control.Monad.Trans.State


makeUniplateDirect :: Derivation
makeUniplateDirect = derivationParams "UniplateDirect" $ \args grab (_,ty) -> simplify $
    let known = map (declName &&& id) knownCtors
        grab2 x = fromMaybe (grab x) $ lookup x known
    in case args of
        _ | not $ null [() | TyVar () _ <- universeBi args] -> error "UniplateDirect only accepts monomorphic types"
        [] -> make True grab2 x x
            where x = tyApps (tyCon $ dataDeclName ty) $ replicate (dataDeclArity ty) $ TyCon () $ Special () (UnitCon ())
        [x] -> make True grab2 x x
        [x,y] -> make False grab2 x y
        _ -> error $ "UniplateDirect requires exactly one or two arguments, got " ++ show (length args)

-- alwaysActive :: Activation ()
-- alwaysActive = ActiveFrom () 0

make :: Bool -> (String -> DataDecl) -> Type () -> Type () -> Either String [Decl ()]
make uni grab from to =
    Right [InstDecl () Nothing instRule
        (Just [InsDecl () $ InlineSig () True Nothing (qname $ if uni then "uniplate" else "biplate"), InsDecl () ms])]
    where
        headName = (UnQual () $ Ident () $ if uni then "Uniplate" else "Biplate")
        instRule = IRule () Nothing Nothing (foldr (flip (IHApp ())) (IHCon () headName) (from : [to | not uni]))
        ty = grab $ tyRoot from
        match pat bod = Match () (Ident () $ if uni then "uniplate" else "biplate") [pat] (UnGuardedRhs () bod) Nothing
        ms = if uni || from /= to
             then FunBind () $ map (uncurry match) (catMaybes bods) ++ [match (pVar "x") (App () (var "plate") (var "x")) | any isNothing bods]
             else PatBind () (pVar "biplate") (UnGuardedRhs () $ var "plateSelf") Nothing
        bods = run (fromTyParens to) $ mapM (make1 grab) $ substData from ty


make1 :: (String -> DataDecl) -> (String,[Type ()]) -> S (Maybe (Pat (), Exp ()))
make1 grab (name,tys) = do
    ops <- mapM (fmap show . operator grab) tys
    let vars = ['x':show i | i <- [1..length tys]]
        pat = PParen () $ PApp () (qname name) $ map pVar vars
        (good,bad) = span ((==) "|-" . fst) $ zip ops $ map var vars
        bod = foldl (\x (y,z) -> InfixApp () x (QVarOp () $ UnQual () $ Symbol () y) z) (App () (var "plate") $ paren $ apps (con name) (map snd good)) bad
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


type S a = State (Map.Map (Type ()) Ans) a

run :: Type () -> S a -> a
run to act = evalState act (Map.singleton to Hit)

operator :: (String -> DataDecl) -> Type () -> S Ans
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


operator2 :: (String -> DataDecl) -> Type () -> S Ans
operator2 grab from
    | isTyFun from = return Try
    | Just from2 <- fromTyList from = fmap ansList $ operator grab from2
    | otherwise = case subst from $ grab $ tyRoot from of
        Left from2 -> operator grab from2
        Right ctrs -> fmap ansJoin $ mapM (operator grab) $ concatMap snd ctrs


subst :: Type () -> Decl () -> Either (Type ()) [(String,[Type ()])]
subst ty x@TypeDecl{} = Left $ substType ty x
subst ty x = Right $ substData ty x

substData :: Type () -> Decl () -> [(String,[Type ()])]
substData ty dat = [(ctorDeclName x, map (fromTyParens . transform f . snd) $ ctorDeclFields x) | x <- dataDeclCtors dat]
    where
        rep = zip (dataDeclVars dat) (snd $ fromTyApps $ fromTyParen ty)
        f (TyVar () x) = fromMaybe (TyVar () x) $ lookup (prettyPrint x) rep
        f x = x

substType :: Type () -> Decl () -> Type ()
substType ty (TypeDecl () dhead d) = fromTyParens $ transform f d
    where
        vars = collect dhead
        rep = zip (map prettyPrint vars) (snd $ fromTyApps ty)
        f (TyVar () x) = fromMaybe (TyVar () x) $ lookup (prettyPrint x) rep
        f x = x
        collect (DHead () _) = []
        collect (DHInfix () bind _) = [bind]
        collect (DHParen () h) = collect h
        collect (DHApp () h bind) = bind : collect h

clearAnn :: Functor f => f a -> f ()
clearAnn = fmap (const ())

knownCtors :: [Decl ()]
knownCtors = map (fromParseResult . fmap clearAnn . parseDecl)
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

listCtor = DataDecl ()  (DataType ()) Nothing (DHApp () (DHead () $ Ident () "[]") (UnkindedVar () $ Ident () "a"))
    [QualConDecl () Nothing Nothing $ ConDecl () (Ident () "[]") []
    ,QualConDecl () Nothing Nothing $ ConDecl () (Ident () "(:)") [tyVar "a", TyList () $ tyVar "a"]] []

tupleDefn :: Int -> Decl ()
tupleDefn n = DataDecl () (DataType ()) Nothing dhead [QualConDecl () Nothing Nothing $ ConDecl () (Ident () s) (map tyVar vars)] []
    where s = "(" ++ replicate (n - 1) ',' ++ ")"
          vars = ['v':show i | i <- [1..n]]
          dhead = foldr (flip (DHApp ())) (DHead () $ Ident () s) (map (UnkindedVar () . Ident ()) vars)
