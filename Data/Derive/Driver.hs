{-# OPTIONS_GHC -fglasgow-exts #-}
-- | The main driver module.  It is intended that this should be the
-- only module imported by user code; it takes care of all data
-- threading issues such that all one needs to do is:
--
-- > data Foo = Foo deriving (Data, Typeable)
-- > main = derive "Eq" (undefined :: Foo)
module Data.Derive.Driver
       (derive, derives, derivable,
        -- $arg
        A(..), B(..), C(..), D(..), E(..)
       ) where

import Data.Generics
import Data.Derive
import Data.List
import Data.Maybe
import Data.Derive.AllDerivers

-- | Derive an instance of some class.  This uses the Scrap Your
-- Boilerplate infrastructure to extract the data type definition; to
-- resolve overloading the second argument to @derive@ is a phantom
-- value of the type you wish the instance to be derived for.  The
-- first argument is the class name.  @derive@ only derives instances
-- for the type of the argument; to derive instances for an entire
-- dependency group of data types, use 'derives'.
derive :: (Data a, Typeable a) => String -> a -> IO ()
derive s x = putStr $ unlines $ getDeriver s $ fromMaybe (error "Cannot derive for this type") (deriveOne x)

-- | @derives@ derives instances of some class for an entire
-- dependency group of data types.  In every other respect it is
-- exactly like 'derive'.
derives :: (Data a, Typeable a) => String -> a -> IO ()
derives s x = putStr $ unlines $ concat $ intersperse [""] $ map (getDeriver s) $ deriveMany x

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
