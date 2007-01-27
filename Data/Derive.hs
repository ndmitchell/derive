{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Derive(
    -- the data type
    DataDef(..), CtorDef(..),
    -- the basic deriver
    deriveOne, deriveMany,
    -- helpers
    instanceHead
    ) where

import Data.Generics
import Data.List
import Data.Maybe


data DataDef = DataDef {dataName :: String, dataFree :: Int, dataCtors :: [CtorDef]}

data CtorDef = CtorDef {ctorName :: String, ctorArity :: Int}

instance Show DataDef where
    show (DataDef name arity ctors) = name ++ " #" ++ show arity ++ (if null ctors then "" else " = ") ++ c
        where c = concat $ intersperse " | " $ map show ctors

instance Show CtorDef where
    show (CtorDef name arity) = name ++ " #" ++ show arity



deriveOne :: (Typeable a, Data a) => a -> Maybe DataDef
deriveOne = fst . deriveInternal


deriveMany :: (Typeable a, Data a) => a -> [DataDef]
deriveMany x = nubBy (\a b -> dataName a == dataName b) $ f [] [DataBox x]
    where
        f _ [] = []
        f seen (DataBox t:odo) | tt `elem` seen = f seen odo
                       | otherwise = maybeToList now ++ f (tt:seen) (rest++odo)
            where
                (now,rest) = deriveInternal t
                tt = typeOf t



data DataBox = forall a . (Typeable a, Data a) => DataBox a

deriveInternal :: (Typeable a, Data a) => a -> (Maybe DataDef, [DataBox])
deriveInternal x = if not $ isAlgType dtyp then (Nothing,[]) else (Just res, concat follow)
    where
        res = DataDef (tyConString typeName) (length typeChildren) ctrs
        (typeName,typeChildren) = splitTyConApp (typeOf x)

        dtyp = dataTypeOf x
        (ctrs,follow) = unzip $ map ctr $ dataTypeConstrs dtyp
        
        ctr :: Constr -> (CtorDef, [DataBox])
        ctr con = (CtorDef (showConstr con) (length childs), childs)
            where
                name = showConstr con
                childs = gmapQ DataBox $ fromConstr con `asTypeOf` x

-- HELPERS

instanceHead :: String -> DataDef -> String
instanceHead cls (DataDef name arity _) =
        "instance " ++
        ['(' | arity > 1] ++
            concat (intersperse ", " [cls ++ " " ++ x | x <- typs]) ++
        [')' | arity > 1] ++
        (if arity > 0 then " => " else "") ++
        cls ++ " " ++
        ['(' | arity > 0] ++
            name ++
            concatMap (' ':) typs ++
        [')' | arity > 0] ++
        " where"
    where
        typs = map (:[]) $ take arity ['a'..]
