-- NOTE: Cannot be guessed as it relies on type information

-- | Derive Play, implemented for tuples and lists.
module Data.Derive.PlateDirect(makePlateDirect) where

import Language.Haskell.TH.All


makePlateDirect :: Derivation
makePlateDirect = derivation plateDirect' "PlateDirect"


plateDirect' :: Dec -> [Dec]
plateDirect' (DataD _ typ [] cs _) =
        [InstanceD [] (l2 "PlateAll" t t) [funN "plateAll" [sclause [] (l0 "plateSelf")]]
        ,InstanceD [] (l1 "PlateOne" t) [funN "plateOne" (map f cs)]
        ]
    where
        t = l0 $ show typ
        
        f x = sclause [ctp x 'x'] $ foldl1 AppE args
            where args = l1 "plate" (l0 $ ctorName x) : zipWith g (ctv x 'x') (ctorTypes x)
        
        g s (AppT (ConT c) t) | show c == "[]" = g s (AppT ListT t)
        g s (AppT ListT (ConT t)) | t == typ  = l1 "||*" s
        g s (AppT ListT _) = l1 "||+" s
        g s (ConT  t) | t == typ  = l1 "|*" s
        g s _ = l1 "|+" s

plateDirect' (NewtypeD a b c d e) = plateDirect' (DataD a b c [d] e)
plateDirect' _ = []

{-
-- an attempt at something better which doesn't really work

getTypes :: Type -> Q [Type]
getTypes t = do
        let (ConT c, cs) = typeApp t
        TyConI dat <- reify c
        return $ concatMap ctorTypes $ dataCtors dat


reaches :: Type -> Q [Type]
reaches t = f [] [t]
    where
        f done [] = return done
        f done (t:odo)
            | t `elem` done = f done odo
            | otherwise = do
                ts <- getTypes t
                f (t:done) (odo ++ ts)


against :: Type -> Type -> Type
against = error "here"

-}
