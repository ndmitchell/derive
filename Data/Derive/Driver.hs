{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Derive.Driver
       (derive, derives, derivable,
        A(..), B(..), C(..), D(..), E(..)
       ) where

import Data.Generics
import Data.Derive
import Data.List
import Data.Maybe

import qualified Data.Derive.Eq
import qualified Data.Derive.BinaryDefer
import qualified Data.Derive.Binary
import qualified Data.Derive.Functor


derivable :: [String]
derivable = map fst derivers

derivers = [("Eq",Data.Derive.Eq.derive)
           ,("BinaryDefer",Data.Derive.BinaryDefer.derive)
           ,("Binary",Data.Derive.Binary.derive)
           ,("Functor",Data.Derive.Functor.derive)
           ]

getDeriver :: String -> (DataDef -> [String])
getDeriver x = fromMaybe (error $ "Do not know how to derive " ++ x) (lookup x derivers)



derive :: (Data a, Typeable a) => String -> a -> IO ()
derive s x = putStr $ unlines $ getDeriver s $ fromMaybe (error "Cannot derive for this type") (deriveOne x)


derives :: (Data a, Typeable a) => String -> a -> IO ()
derives s x = putStr $ unlines $ concat $ intersperse [""] $ map (getDeriver s) $ deriveMany x


-- | Arguments for type contructors
data A = A deriving (Typeable, Data, Show, Eq)
data B = B deriving (Typeable, Data, Show, Eq)
data C = C deriving (Typeable, Data, Show, Eq)
data D = D deriving (Typeable, Data, Show, Eq)
data E = E deriving (Typeable, Data, Show, Eq)
