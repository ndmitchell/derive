
module Apply where

import HSE
import DSL


apply :: Dat -> DSL -> Res
apply dat dsl = fromUniverse $ applyData dat dsl


applyData :: Dat -> DSL -> Universe
applyData dat (Instance ctx hd body) =
        UApp "InstDecl" [uni con, uni $ UnQual $ Ident hd, uni [typ], applyData dat body]
    where
        con = [ClassA (UnQual $ Ident c) [TyVar $ Ident v] | v <- dataVars dat, c <- ctx]
        typ = foldl TyApp (TyCon $ UnQual $ Ident $ dataName dat) [TyVar $ Ident v | v <- dataVars dat]
applyData dat (MapCtor dsl) = UList $ map (`applyCtor` dsl) $ dataCtors dat
applyData dat dsl = applyAll (applyData dat) dsl


applyCtor :: Ctr -> DSL -> Universe
applyCtor ctr CtorName = UString $ ctorName ctr
applyCtor ctr dsl = applyAll (applyCtor ctr) dsl


applyAll :: (DSL -> Universe) -> DSL -> Universe
applyAll f (List xs) = UList $ map f xs
applyAll f (Append xs ys) = UList $ fromList (f xs) ++ fromList (f ys)
applyAll f (App x ys) = UApp x $ map f ys
applyAll f dsl = error $ "applyAll: " ++ show dsl


fromList (UList x) = x
fromList x = error $ "Expecting a list but not found, " ++ show x
