{-# LANGUAGE GADTs #-}

module DSL where

import Language.Haskell.Exts

type Data = Decl
type Ctor = QualConDecl


dataVars :: Data -> [String]
dataVars (DataDecl _ _ _ _ x _ _) = map prettyPrint x

dataName :: Data -> String
dataName (DataDecl _ _ _ x _ _ _) = prettyPrint x


---------------------------------------------------------------------
-- DSL

-- from `elem` Data Ctor i (parameter type)
-- to `elem` HSE (result type)
data DSL from to where
    C :: to -> DSL i to
    Ap :: DSL i (a -> b) -> DSL i a -> DSL i b

    Instance :: [String] -> String -> DSL Data [InstDecl] -> DSL Data Decl



evalData :: DSL Data a -> Data -> a
evalData (C a) x = a
evalData (Ap a b) x = evalData a x $ evalData b x

evalData (Instance ctx hd body) x =
        InstDecl sl con (UnQual $ Ident hd) [typ] (evalData body x)
    where
        con = [ClassA (UnQual $ Ident c) [TyVar $ Ident v] | v <- dataVars x, c <- ctx]
        typ = foldl TyApp (TyCon $ UnQual $ Ident $ dataName x) [TyVar $ Ident v | v <- dataVars x]
        

data Sample a = Zero | One a | Two a a | Three a a


singleton :: DSL i a -> DSL i [a]
singleton x = Ap (C (:[])) x


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


eq :: DSL Data [Decl]
eq = singleton $ Instance ["Eq"] "Eq" (C [])


test = putStr $ unlines $ map prettyPrint $ evalData eq list
