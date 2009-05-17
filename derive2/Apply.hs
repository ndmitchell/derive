
module Apply where

import HSE
import DSL
import Data.Maybe
import Data.List


apply :: DSL -> Input -> Out
apply dsl input = fromOutput $ apply2 input Nothing Nothing Nothing dsl


-- ctor is 0 based index, field is 1 based
apply2 :: Input -> Maybe Int -> Maybe Int -> Maybe (Output,Output) -> DSL -> Output
apply2 input ctr fld lst = f
    where
        dataVariables = genericTake (dataVars input) $ map (:[]) ['a'..]
    
        ctr2 = dataCtors input !! fromJust ctr
        app2 = apply2 input ctr fld lst
    
        f (Instance ctx hd body) =
                OApp "InstDecl" [out con, out $ UnQual $ Ident hd, out [typ], app2 body]
            where
                con = [ClassA (UnQual $ Ident c) [TyVar $ Ident v] | v <- dataVariables, c <- ctx]
                typ = foldl TyApp (TyCon $ UnQual $ Ident $ dataName input) [TyVar $ Ident v | v <- dataVariables]
        
        f (Application xs) = case app2 xs of
            OList xs -> g xs
            where g [x] = x
                  g xs = OApp "App" [g $ init xs, last xs]

        f (MapCtor dsl) = OList [apply2 input (Just c) Nothing lst dsl | c <- [0 .. length (dataCtors input) - 1]]
        f (MapField dsl) = OList [apply2 input ctr (Just i) lst dsl | i <- [1.. fromIntegral $ ctorArity ctr2]]

        f DataName = OString $ dataName input
        f CtorName = OString $ ctorName ctr2
        f CtorArity = OInt $ fromIntegral $ ctorArity ctr2
        f CtorInd = OInt $ fromIntegral $ fromJust ctr
        f FieldInd = OInt $ fromIntegral $ fromJust fld

        f Head = fst $ fromJust lst
        f Tail = snd $ fromJust lst
        f (Fold cons xs) = case app2 xs of OList xs -> g xs
            where g [x] = x
                  g (x:xs) = apply2 input ctr fld (Just (x,g xs)) cons

        f (List xs) = OList $ map f xs
        f (Reverse xs) = case app2 xs of
            OList xs -> OList $ reverse xs
        f (Concat xs) = case app2 xs of
            OList [] -> OList []
            OList xs -> foldr1 g xs
        f (String x) = OString x
        f (Int x) = OInt x
        f (ShowInt x) = case app2 x of OInt x -> OString (show x)
        f (App x ys) = case app2 ys of
            OList ys -> OApp x ys

        g (OList x) (OList y) = OList (x++y)
        g (OString x) (OString y) = OString (x++y)


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
        f CtorInd = OInt $ ctorIndex ctor
        f FieldInd = OInt $ field

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
