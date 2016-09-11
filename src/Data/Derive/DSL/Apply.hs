{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Data.Derive.DSL.Apply(apply, applyEnv, env, Env(..)) where

import Data.Derive.DSL.HSE
import Data.Derive.DSL.DSL
import Data.List
import Data.Generics.Uniplate.DataOnly


apply :: DSL -> Input -> Out
apply dsl input = fromOutput $ applyEnv dsl env{envInput=input}


env = Env
    (error "Env.envInput: uninitialised")
    (error "Env.envCtor: uninitialised")
    (error "Env.envField: uninitialised")
    (error "Env.envFold: uninitialised")


data Env = Env  {envInput :: Input
                ,envCtor :: Ctor
                ,envField :: Integer
                ,envFold :: (Output,Output) }

applyEnv :: DSL -> Env -> Output
applyEnv dsl env@(Env input ctor field fold) = f dsl
    where
    f (Instance ctx hd body) =
        OApp "InstDecl"
            [out ()
            ,out (Nothing :: Maybe (Overlap ()))
            ,out (IRule () Nothing context insthead :: InstRule ())
            ,f body]
        where
            context = Just $ CxTuple ()
                [ClassA () (UnQual () $ Ident () c) [TyVar () $ Ident () v]
                | let seen = [x | TyVar () (Ident () x) <- universeBi $ concatMap ctorDeclFields $ dataCtors input]
                , v <- dataDeclVarsStar input `intersect` seen
                , c <- ctx]
            ty = TyParen () $ foldl (TyApp ())
                (TyCon () $ UnQual () $ Ident () $ dataName input)
                (map tyVar $ dataDeclVars input)
            insthead = IHApp () (IHCon () $ UnQual () $ Ident () hd) ty

    f (Application (f -> OList xs)) =
        foldl1 (\a b -> OApp "App" [OApp "()" [],a,b]) xs

    f (MapCtor dsl) = OList  [applyEnv dsl env{envCtor=c}
         |  c <- dataCtors input]
    f (MapField dsl) = OList [applyEnv dsl env{envField=i}
         | i <- [1.. fromIntegral $ ctorArity ctor]]

    f DataName = OString $ dataName input
    f CtorName = OString $ ctorName ctor
    f CtorArity = OInt $ ctorArity ctor
    f CtorIndex = OInt $ ctorIndex input ctor
    f FieldIndex = OInt $ field

    f Head = fst fold
    f Tail = snd fold
    f (Fold cons (f -> OList xs)) =
        foldr1 (\a b -> applyEnv cons env{envFold=(a,b)}) xs

    f (List xs) = OList $ map f xs
    f (Reverse (f -> OList xs)) = OList $ reverse xs
    f (Concat (f -> OList [])) = OList []
    f (Concat (f -> OList xs)) = foldr1 g xs
            where  g (OList    x) (OList    y) = OList    (x++y)
                   g (OString  x) (OString  y) = OString  (x++y)
    f (String x) = OString x
    f (Int x) = OInt x
    f (ShowInt (f -> OInt x)) = OString $ show x
    f (App x (f -> OList ys)) = OApp x ys
