{-# LANGUAGE ExistentialQuantification #-}

module Data.Derive.Internal.Instance(
    Data, d_ctorArity, d_ctorValue, d_dataCtors
    ) where

import Data.Data
import Control.Monad
import Control.Monad.State


data Ctor = Ctor
    {ctorType :: Box
    ,ctorIndex :: Int
    ,ctorRep :: Constr}


data Box = forall a . Data a => Box a


d_ctorArity :: Ctor -> Int
d_ctorArity Ctor{ctorType=Box t, ctorRep=rep} = flip execState 0 $
    liftM (`asTypeOf` t) $
    fromConstrM (modify (+1) >> return undefined) rep


d_ctorValue :: Data a => Ctor -> a
d_ctorValue = error "TODO: d_ctorValue"


d_dataCtors :: Data a => a -> [Ctor]
d_dataCtors x
    | not $ isAlgType t = error "d_dataCtors only works on algebraic data types"
    | otherwise = zipWith (Ctor $ Box x) [0..] $ dataTypeConstrs t
    where t = dataTypeOf x
