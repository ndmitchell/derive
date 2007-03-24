{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Derive(
    -- the data type
    DataDef(..), CtorDef(..),
    Type(..), TypeCon(..),
    -- the basic deriver
    deriveOne, deriveMany,
    -- helpers
    instanceHead
    ) where

import Data.Generics
import Data.List
import Data.Maybe
import Data.Char


data DataDef = DataDef {dataName :: String, dataFree :: Int, dataCtors :: [CtorDef]}
	deriving (Eq, Ord)

data CtorDef = CtorDef {ctorName :: String, ctorArity :: Int, ctorTypes :: [Type]}
	deriving (Eq, Ord)

data Type     = Type  {typeCon :: TypeCon, typeArgs :: [Type] }
	deriving (Eq, Ord)

data TypeCon = TypeCon String
             | TypeArg  Int -- argument of a DataDef
	deriving (Eq, Ord)

instance Show DataDef where
    show (DataDef name arity ctors) = name ++ " #" ++ show arity ++ (if null ctors then "" else " = ") ++ c
        where c = concat $ intersperse " | " $ map show ctors

instance Show CtorDef where
    show (CtorDef name arity ts) = name ++ " #" ++ show arity ++ " : " ++ show ts

instance Show Type where
    show (Type con [])   = show con
    show (Type con args) = "(" ++ show con ++ concatMap ((" "++) . show) args ++ ")"

instance Show TypeCon where
    show (TypeCon n) = n
    show (TypeArg i) = [chr (ord 'a' + i)]



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
        
        toType :: DataBox -> Type
        toType (DataBox a) = repToType (typeOf a)
        repToType :: TypeRep -> Type
        repToType tr = let (f,as) = splitTyConApp tr
                       in Type (conToType f) (map repToType as)
        conToType :: TyCon -> TypeCon
        conToType tc
         | Just a <- lookup tc args  =  TypeArg a
         | otherwise                 =  TypeCon (tyConString tc)
            where
                args = zip (map typeRepTyCon typeChildren) [0..]
        
        ctr :: Constr -> (CtorDef, [DataBox])
        ctr con = (CtorDef (showConstr con) (length childs) (map toType childs), childs)
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
