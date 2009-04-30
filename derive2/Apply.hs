
module Apply where

import HSE
import DSL
import Data.Maybe


apply :: Dat -> DSL -> Res
apply dat dsl = fromUniverse $ apply2 dat Nothing Nothing dsl


apply2 :: Dat -> Maybe Ctr -> Maybe Int -> DSL -> Universe
apply2 dat ctr fld = f
    where
        app2 = apply2 dat ctr fld
    
        f (Instance ctx hd body) =
                UApp "InstDecl" [uni con, uni $ UnQual $ Ident hd, uni [typ], app2 body]
            where
                con = [ClassA (UnQual $ Ident c) [TyVar $ Ident v] | v <- dataVars dat, c <- ctx]
                typ = foldl TyApp (TyCon $ UnQual $ Ident $ dataName dat) [TyVar $ Ident v | v <- dataVars dat]

        f (MapCtor dsl) = UList [apply2 dat (Just c) Nothing dsl | c <- dataCtors dat]
        f (MapField dsl) = UList [apply2 dat ctr (Just i) dsl | i <- [1..ctorFields $ fromJust ctr]]

        f CtorName = UString $ ctorName $ fromJust ctr
        f FieldInd = UInt $ fromJust fld

        f (List xs) = UList $ map f xs
        f (Append x y) = case (app2 x, app2 y) of
            (UList x, UList y) -> UList (x++y)
            (UString x, UString y) -> UString (x++y)
        f (String x) = UString x
        f (ShowInt x) = case app2 x of
            UInt x -> UString (show x)
        f (App x ys) = UApp x $ map app2 ys
        f dsl = error $ "app: " ++ show dsl
