{-# OPTIONS_GHC -fglasgow-exts #-}
-- | The main driver module.  It is intended that this should be the
-- only module imported by user code; it takes care of all data
-- threading issues such that all one needs to do is:
--
-- @
--   data Foo = Foo deriving (Data, Typeable)
--   main = derive 'Data.Derive.StdDerivations.eq' (undefined :: Foo)
-- @
module Data.Derive.Driver
       (derive, derives,
        -- $arg
        A(..), B(..), C(..), D(..), E(..),
        -- * Convienience re-exports
        Derivation, -- abstract!
       ) where

import Data.Generics
import Data.Derive
import Data.List
import Data.Maybe

-- | Derive an instance of some class.  This uses the Scrap Your
-- Boilerplate infrastructure to extract the data type definition; to
-- resolve overloading the second argument to @derive@ is a phantom
-- value of the type you wish the instance to be derived for.  The
-- first argument is the class name.  @derive@ only derives instances
-- for the type of the argument; to derive instances for an entire
-- dependency group of data types, use 'derives'.
derive :: (Data a, Typeable a) => Derivation -> a -> IO ()
derive (Derivation f) x = putStr $ unlines $ f $ fromMaybe (error "Cannot derive for this type") (deriveOne x)

-- | @derives@ derives instances of some class for an entire
-- dependency group of data types.  In every other respect it is
-- exactly like 'derive'.
derives :: (Data a, Typeable a) => Derivation -> a -> IO ()
derives (Derivation f) x = putStr $ unlines $ concat $ intersperse [""] $ map f $ deriveMany x

-- | Extract a 'DataDef' value from a type using the SYB framework.  A
-- phantom type argument is required to specifiy the type.  Returns
-- 'Nothing' if the passed type is not algebraic.
deriveOne :: (Typeable a, Data a) => a -> Maybe DataDef
deriveOne = fst . deriveInternal

-- | Extract 'DataDef's for each algebraic type referenced (directly
-- or otherwise) from the phantom type argument.
deriveMany :: (Typeable a, Data a) => a -> [DataDef]
deriveMany x = nubBy (\a b -> dataName a == dataName b) $ f [] [DataBox x]
    where
        f _ [] = []
        f seen (DataBox t:odo) | tt `elem` seen = f seen odo
                       | otherwise = maybeToList now ++ f (tt:seen) (rest++odo)
            where
                (now,rest) = deriveInternal t
                tt = typeOf t

-- | An existential box representing a type which supports SYB
-- operations.
data DataBox = forall a . (Typeable a, Data a) => DataBox a

-- | Given a phantom type, extract the 'DataDef' itself and boxes
-- representing all /directly/ referenced types.  Used to implement
-- 'deriveOne' and 'deriveMany'.
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

-- $arg
--
-- These types are intended to be used to resolve overloading issues
-- caused by the restriction of phantom type arguments to kind *.  For
-- instance, to derive an instance of @Eq@ for the standard list type,
-- @derive "Eq" (undefined :: [a])@ will not work because the compiler
-- is unaware derive does not use the type arguments of @[]@; but you
-- can use @derive "Eq" (undefined :: [A])@.
data A = A deriving (Typeable, Data, Show, Eq)
data B = B deriving (Typeable, Data, Show, Eq)
data C = C deriving (Typeable, Data, Show, Eq)
data D = D deriving (Typeable, Data, Show, Eq)
data E = E deriving (Typeable, Data, Show, Eq)
