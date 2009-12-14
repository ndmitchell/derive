-- NOTE: Cannot be guessed as it relies on type information

-- | Derive Uniplate and Biplate using the Direct combinators.
--   You must specific monomorphic instances, i.e:
--
-- > data Foo a = Foo a Int
-- >      deriving ({-! PlateDirect (Foo Int), PlateDirect (Foo Int) Int !-})
--
--   All types referred to must be in scope at the time.
module Data.Derive.PlateDirect(makePlateDirect) where

{-
-}

{-
import Data.Generics.PlateDirect

test :: PlateDirect (Sample Int)

instance Uniplate (Sample Int) where
    uniplate (First) = plate First
    uniplate (Second x1 x2) = plate Second |* x1 |* x2
    uniplate (Third x1) = plate Third |* x1


test :: PlateDirect (Sample Int) Int

instance Biplate Sample Int where
    biplate x = plate x

test :: PlateDirect Computer

instance Uniplate Computer where
    uniplate x = plate x

test :: PlateDirect Computer Double

instance Biplate Computer Double where
    biplate (Laptop x1 x2) = plate Laptop |* x1 |- x2
    biplate x = x

-}

import Language.Haskell
import Data.Generics.PlateData
import Data.Derive.Internal.Derivation


makePlateDirect :: Derivation
makePlateDirect = derivationParams "PlateDirect" $ \args grab (_,ty) ->
    case args of
        _ | not $ null [() | TyVar _ <- universeBi args] -> error "PlateDirect only accepts monomorphic types"
        [x] -> Right [InstDecl sl [] (UnQual $ Ident "Uniplate") [x] $ make (snd . grab) ty x x]
        [x,y] -> Right [InstDecl sl [] (UnQual $ Ident "Biplate") [x,y] $ make (snd . grab) ty x y]
        _ -> error $ "PlateDirect requires exactly one or two arguments, got " ++ show (length args)


make :: (String -> DataDecl) -> DataDecl -> Type -> Type -> [InstDecl]
make _ _ _ _ = []


{-

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
-}

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
