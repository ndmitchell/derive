
module SYB where

import HSE
import DSL


dslSYB :: DSL -> Maybe Out
dslSYB (List [Instance _ name bod]) = Nothing {- do
    bod <- f bod
    InstDecl [] (UnQual $ Ident name) ( `liftM` generateArities input = [InstDecl []
    (UnQual $ Ident "Arities")
    (foldl TyApp
        (TyCon $ UnQual $ Ident $ dataName input)
        (map (TyVar . Ident) vars))
    [InsDecl [FunBind [Match [
        App (Ident "arities") [PWildCard] Nothing ellipses]]]]
    where vars = take (dataVars input) $ map (:[]) ['a'..]
-}
