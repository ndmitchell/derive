
module Apply(apply, applyEnv, Env(..)) where

import HSE
import DSL
import Data.Maybe
import Data.List


apply :: DSL -> Input -> Out
apply dsl input = fromOutput $ applyEnv dsl Env{envInput=input}


data Env = Env
    {envInput :: Input
    ,envCtor :: Ctor
    ,envField :: Integer
    ,envFold :: (Output,Output)
    }

applyEnv :: DSL -> Env -> Output
applyEnv dsl env@(Env input ctor field fold) = f dsl
    where
        dataVariables = genericTake (dataVars input) $ map (:[]) ['a'..]
    
        f (Instance ctx hd body) =
                OApp "InstDecl" [out con, out $ UnQual $ Ident hd, out [typ], f body]
            where
                con = [ClassA (UnQual $ Ident c) [TyVar $ Ident v] | v <- dataVariables, c <- ctx]
                typ = foldl TyApp (TyCon $ UnQual $ Ident $ dataName input) [TyVar $ Ident v | v <- dataVariables]
        
        f (Application xs) = case f xs of
            OList xs -> g xs
            where g [x] = x
                  g xs = OApp "App" [g $ init xs, last xs]

        f (MapCtor dsl) = OList [applyEnv dsl env{envCtor=c} | c <- dataCtors input]
        f (MapField dsl) = OList [applyEnv dsl env{envField=i} | i <- [1.. fromIntegral $ ctorArity ctor]]

        f DataName = OString $ dataName input
        f CtorName = OString $ ctorName ctor
        f CtorArity = OInt $ ctorArity ctor
        f CtorIndex = OInt $ ctorIndex ctor
        f FieldIndex = OInt $ field

        f Head = fst fold
        f Tail = snd fold
        f (Fold cons xs) = case f xs of OList xs -> g xs
            where g [x] = x
                  g (x:xs) = applyEnv cons env{envFold=(x,g xs)}

        f (List xs) = OList $ map f xs
        f (Reverse xs) = case f xs of
            OList xs -> OList $ reverse xs
        f (Concat xs) = case f xs of
            OList [] -> OList []
            OList xs -> foldr1 g xs
        f (String x) = OString x
        f (Int x) = OInt x
        f (ShowInt x) = case f x of OInt x -> OString (show x)
        f (App x ys) = case f ys of
            OList ys -> OApp x ys

        g (OList x) (OList y) = OList (x++y)
        g (OString x) (OString y) = OString (x++y)
