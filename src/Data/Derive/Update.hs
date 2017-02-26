{-|
    A Pseudo derivation. For every label, creates a function
    foo_u and foo_s which updates and sets the label respectively,
    e.g. 'foo_u (+1) bar' or 'foo_s 10 baz'
-}
module Data.Derive.Update(makeUpdate) where

{-

test :: Computer

speed_u :: (Int -> Int) -> Computer -> Computer
speed_u f x = x{speed = f (speed x)}

speed_s :: Int -> Computer -> Computer
speed_s v x = x{speed = v}

weight_u :: (Double -> Double) -> Computer -> Computer
weight_u f x = x{weight = f (weight x)}

weight_s :: Double -> Computer -> Computer
weight_s v x = x{weight = v}

test :: Sample

-}

import Language.Haskell
import Data.Derive.Internal.Derivation
import Data.Maybe


makeUpdate :: Derivation
makeUpdate = derivationCustom "Update" $ \(_,d) -> Right $ concatMap (makeUpdateField d) $ dataDeclFields d


makeUpdateField :: DataDecl -> String -> [Decl ()]
makeUpdateField d field =
        [TypeSig () [name upd] (TyFun () (TyParen () (TyFun () typF typF)) typR)
        ,bind upd [pVar "f",pVar "x"] $ RecUpdate () (var "x") [FieldUpdate () (qname field) (App () (var "f") (Paren () $ App () (var field) (var "x")))]
        ,TypeSig () [name set] (TyFun () typF typR)
        ,bind set [pVar "v",pVar "x"] $ RecUpdate () (var "x") [FieldUpdate () (qname field) (var "v")]]
    where
        set = field ++ "_s"
        upd = field ++ "_u"
        typR = TyFun () (dataDeclType d) (dataDeclType d)
        typF = fromJust $ lookup field $ concatMap ctorDeclFields $ dataDeclCtors d
