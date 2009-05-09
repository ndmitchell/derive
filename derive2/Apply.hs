
module Apply where

import HSE
import DSL
import Data.Maybe


apply :: Dat -> DSL -> Res
apply dat dsl = fromUniverse $ apply2 dat Nothing Nothing Nothing dsl


-- ctor is 0 based index, field is 1 based
apply2 :: Dat -> Maybe Int -> Maybe Int -> Maybe (Universe,Universe) -> DSL -> Universe
apply2 dat ctr fld lst = f
    where
        ctr2 = dataCtors dat !! fromJust ctr
        app2 = apply2 dat ctr fld lst
    
        f (Instance ctx hd body) =
                UApp "InstDecl" [uni con, uni $ UnQual $ Ident hd, uni [typ], app2 body]
            where
                con = [ClassA (UnQual $ Ident c) [TyVar $ Ident v] | v <- dataVars dat, c <- ctx]
                typ = foldl TyApp (TyCon $ UnQual $ Ident $ dataName dat) [TyVar $ Ident v | v <- dataVars dat]

        f (MapCtor dsl) = UList [apply2 dat (Just c) Nothing lst dsl | c <- [0 .. length (dataCtors dat) - 1]]
        f (MapField dsl) = UList [apply2 dat ctr (Just i) lst dsl | i <- [1..ctorFields ctr2]]

        f CtorName = UString $ ctorName ctr2
        f FieldInd = UInt $ fromJust fld

        f Head = fst $ fromJust lst
        f Tail = snd $ fromJust lst
        f (Fold cons nil xs) = case app2 xs of UList xs -> g xs
            where
                g [] = app2 nil
                g (x:xs) = apply2 dat ctr fld (Just (x,g xs)) cons

        f (List xs) = UList $ map f xs
        f (Concat xs) = case app2 xs of
            UList [] -> UList []
            UList xs -> foldr1 g xs
        f (String x) = UString x
        f (ShowInt x) = case app2 x of UInt x -> UString (show x)
        f (App x ys) = case app2 ys of
            UList ys -> UApp x ys
        f dsl = error $ "app: " ++ show dsl

        g (UList x) (UList y) = UList (x++y)
        g (UString x) (UString y) = UString (x++y)
