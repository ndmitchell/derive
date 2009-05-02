
module Apply where

import HSE
import DSL
import Data.Maybe


apply :: Dat -> DSL -> Res
apply dat dsl = fromUniverse $ apply2 dat Nothing Nothing Nothing dsl


apply2 :: Dat -> Maybe Ctr -> Maybe Int -> Maybe (Universe,Universe) -> DSL -> Universe
apply2 dat ctr fld lst = f
    where
        app2 = apply2 dat ctr fld lst
    
        f (Instance ctx hd body) =
                UApp "InstDecl" [uni con, uni $ UnQual $ Ident hd, uni [typ], app2 body]
            where
                con = [ClassA (UnQual $ Ident c) [TyVar $ Ident v] | v <- dataVars dat, c <- ctx]
                typ = foldl TyApp (TyCon $ UnQual $ Ident $ dataName dat) [TyVar $ Ident v | v <- dataVars dat]

        f (MapCtor dsl) = UList [apply2 dat (Just c) Nothing lst dsl | c <- dataCtors dat]
        f (MapField dsl) = UList [apply2 dat ctr (Just i) lst dsl | i <- [1..ctorFields $ fromJust ctr]]

        f CtorName = UString $ ctorName $ fromJust ctr
        f FieldInd = UInt $ fromJust fld

        f Head = fst $ fromJust lst
        f Tail = snd $ fromJust lst
        f (Fold cons nil xs) = case app2 xs of UList xs -> g xs
            where
                g [] = app2 nil
                g (x:xs) = apply2 dat ctr fld (Just (x,g xs)) cons

        f (List xs) = UList $ map f xs
        f (Append x y) = case (app2 x, app2 y) of
            (UList x, UList y) -> UList (x++y)
            (UString x, UString y) -> UString (x++y)
        f (String x) = UString x
        f (ShowInt x) = case app2 x of UInt x -> UString (show x)
        f (App x ys) = UApp x $ map app2 ys
        f dsl = error $ "app: " ++ show dsl
