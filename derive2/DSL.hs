{-# LANGUAGE GADTs, RankNTypes #-}

module DSL where

import Language.Haskell.Exts

type Data = Decl
type Ctor = QualConDecl


dataVars :: Data -> [String]
dataVars (DataDecl _ _ _ _ x _ _) = map prettyPrint x

dataName :: Data -> String
dataName (DataDecl _ _ _ x _ _ _) = prettyPrint x

dataCtors :: Data -> [Ctor]
dataCtors (DataDecl _ _ _ _ _ x _) = x


ctorName :: Ctor -> String
ctorName (QualConDecl _ _ _ x) = case x of
    ConDecl n _ -> prettyPrint n
    RecDecl n _ -> prettyPrint n

ctorArity :: Ctor -> Int
ctorArity (QualConDecl _ _ _ x) = case x of
    ConDecl _ x -> length x
    RecDecl _ x -> length $ concatMap fst x


{-
---------------------------------------------------------------------
-- DSL

-- from `elem` Data Ctor Int i (parameter type)
-- to `elem` HSE (result type)
data DSL from to where
    C :: to -> DSL i to
    Ap :: DSL i (a -> b) -> DSL i a -> DSL i b

{ -
    Reverse :: DSL i [a] -> DSL i [a]
    Foldr :: DSL i (a -> a -> a) -> DSL i [a] -> DSL i a
    Foldl :: DSL i (a -> a -> a) -> DSL i [a] -> DSL i a
- }

    Ctors :: DSL Ctor a -> DSL Data [a]
    Arity :: DSL Int a -> DSL Ctor [a]

    CtorName :: DSL Ctor String
    CtorTag :: DSL Ctor Int
    CtorArity :: DSL Ctor Int

    Instance :: [String] -> String -> DSL Data [InstDecl] -> DSL Data Decl


evalData :: DSL Data a -> Data -> a
evalData (Ctors a) x = map (evalCtor a) (dataCtors x)

evalData (Instance ctx hd body) x =
        InstDecl sl con (UnQual $ Ident hd) [typ] (evalData body x)
    where
        con = [ClassA (UnQual $ Ident c) [TyVar $ Ident v] | v <- dataVars x, c <- ctx]
        typ = foldl TyApp (TyCon $ UnQual $ Ident $ dataName x) [TyVar $ Ident v | v <- dataVars x]

evalData a x = evalAny (`evalData` x) a


evalCtor :: DSL Ctor a -> Ctor -> a
evalCtor a x = evalAny (`evalCtor` x) a


evalAny :: (forall a . DSL i a -> a) -> DSL i b -> b
evalAny f (C a) = a
evalAny f (Ap a b) = f a $ f b

lift0 f = C f
lift1 f x1 = C f `Ap` x1
lift2 f x1 x2 = C f `Ap` x1 `Ap` x2
lift3 f x1 x2 x3 = C f `Ap` x1 `Ap` x2 `Ap` x3
lift4 f x1 x2 x3 x4 = C f `Ap` x1 `Ap` x2 `Ap` x3 `Ap` x4
lift5 f x1 x2 x3 x4 x5 = C f `Ap` x1 `Ap` x2 `Ap` x3 `Ap` x4 `Ap` x5


---------------------------------------------------------------------
-- LIFTED LIST

nil :: DSL i [a]
nil = lift0 []

box :: DSL i a -> DSL i [a]
box = lift1 (:[])

snoc :: DSL i [a] -> DSL i a -> DSL i [a]
snoc = lift2 (\x y -> x ++ [y])

---------------------------------------------------------------------
-- LIFTED HSE

insDecl = lift1 InsDecl
funBind = lift1 FunBind
match = lift5 (Match sl)

-}

instance_ dat ctx hd body =
        InstDecl sl con (UnQual $ Ident hd) [typ] body
    where
        con = [ClassA (UnQual $ Ident c) [TyVar $ Ident v] | v <- dataVars dat, c <- ctx]
        typ = tyApps (TyCon $ UnQual $ Ident $ dataName dat) [TyVar $ Ident v | v <- dataVars dat]



data Sample a = Zero | One a | Two a a | Three a a

(+:) a b = a ++ [b]
box = (:[])

tyApps = foldl TyApp
apps = foldl App


---------------------------------------------------------------------
-- EXAMPLES

sl = SrcLoc "" 0 0

-- data List a = Nil | Cons a (List a)
list :: Data
list = DataDecl sl DataType [] (Ident "List") [Ident "a"] [nil,cons] []
    where
        nil = QualConDecl sl [] [] $ ConDecl (Ident "Nil") []
        cons = QualConDecl sl [] [] $ ConDecl (Ident "Cons")
            [UnBangedTy $ TyVar $ Ident "a", UnBangedTy $ TyApp (TyCon $ UnQual $ Ident "List") (TyVar $ Ident "a")]


eq :: Data -> [Decl]
eq dat = box $ instance_ dat ["Eq"] "Eq" [InsDecl $ FunBind $ map op (dataCtors dat) +: def]
    where
        def = Match sl (Symbol "==") [PWildCard,PWildCard] Nothing (UnGuardedRhs $ Con $ UnQual $ Ident "False") (BDecls [])
        op ctr = Match sl (Symbol "==") [c 'x' ctr,c 'y' ctr] Nothing (UnGuardedRhs $ Con $ UnQual $ Ident "True") (BDecls [])

        c pre ctr = PApp (UnQual $ Ident $ ctorName ctr) [PVar $ Ident $ pre:show i | i <- [1..ctorArity ctr]]


test = putStr $ unlines $ map prettyPrint $ eq list
