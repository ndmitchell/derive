
module Apply where

import HSE
import DSL
import Data.Maybe
import Data.List


apply :: Input -> DSL -> Out
apply input dsl = fromOutput $ apply2 input Nothing Nothing Nothing dsl


-- ctor is 0 based index, field is 1 based
apply2 :: Input -> Maybe Int -> Maybe Int -> Maybe (Output,Output) -> DSL -> Output
apply2 dat ctr fld lst = f
    where
        dataVariables = genericTake (dataVars dat) $ map (:[]) ['a'..]
    
        ctr2 = dataCtors dat !! fromJust ctr
        app2 = apply2 dat ctr fld lst
    
        f (Instance ctx hd body) =
                OApp "InstDecl" [out con, out $ UnQual $ Ident hd, out [typ], app2 body]
            where
                con = [ClassA (UnQual $ Ident c) [TyVar $ Ident v] | v <- dataVariables, c <- ctx]
                typ = foldl TyApp (TyCon $ UnQual $ Ident $ dataName dat) [TyVar $ Ident v | v <- dataVariables]
        
        f (Application xs) = case app2 xs of
            OList xs -> g xs
            where g [x] = x
                  g xs = OApp "App" [g $ init xs, last xs]

        f (MapCtor dsl) = OList [apply2 dat (Just c) Nothing lst dsl | c <- [0 .. length (dataCtors dat) - 1]]
        f (MapField dsl) = OList [apply2 dat ctr (Just i) lst dsl | i <- [1.. fromIntegral $ ctorArity ctr2]]

        f DataName = OString $ dataName dat
        f CtorName = OString $ ctorName ctr2
        f CtorArity = OInt $ fromIntegral $ ctorArity ctr2
        f CtorInd = OInt $ fromIntegral $ fromJust ctr
        f FieldInd = OInt $ fromIntegral $ fromJust fld

        f Head = fst $ fromJust lst
        f Tail = snd $ fromJust lst
        f (Fold cons xs) = case app2 xs of OList xs -> g xs
            where g [x] = x
                  g (x:xs) = apply2 dat ctr fld (Just (x,g xs)) cons

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
        f dsl = error $ "app: " ++ show dsl

        g (OList x) (OList y) = OList (x++y)
        g (OString x) (OString y) = OString (x++y)
