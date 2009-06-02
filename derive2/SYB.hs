{-# LANGUAGE RelaxedPolyRec, RankNTypes, ScopedTypeVariables #-}
{- OPTIONS_GHC -fglasgow-exts -}

module SYB(dslSYB) where

import HSE
import DSL
import Control.Monad
import Control.Monad.State hiding (lift)
import Data.Generics
import Data.Maybe


dslSYB :: DSL -> Maybe Out
dslSYB = syb


syb :: Data a => DSL -> Maybe a
syb = dsimple & dlistAny & dapp & derr

lift :: (Data a, Data b) => (DSL -> Maybe b) -> (DSL -> Maybe a)
lift f = maybe Nothing id . cast . f

(&) a b x = a x `mplus` b x


dlistAny :: forall a . Data a => DSL -> Maybe a
dlistAny x | isNothing con = Nothing
           | otherwise = res 
    where
        con = readConstr dat "(:)"
        val = fromConstr (fromJust con) :: a
        
        dat = dataTypeOf (undefined :: a)
        
        res = gmapQi 0 f val

        f :: Data d => d -> Maybe a
        f y = fromJust $ cast $ dlist x `asTypeOf` Just [y]


dlist :: Data a => DSL -> Maybe [a]
dlist x = do
    List xs <- return x
    mapM syb xs


dapp :: forall a . Data a => DSL -> Maybe a
dapp x = do
    App name (List args) <- return x
    let dat = dataTypeOf (undefined :: a)
        (res,s) = runState (fromConstrM f $ readCon dat name) (True,args)
    if fst s then Just res else Nothing
    where
        f :: forall b . Data b => State (Bool,[DSL]) b
        f = if typeOf (undefined :: b) == typeOf sl then return $ coerce sl
            else do
                (b,x:xs) <- get
                case syb x of
                    Nothing -> do put (False,xs) ; return undefined
                    Just y -> do put (b,xs) ; return y


derr :: Data a => DSL -> Maybe a
derr x = error $ "Couldn't dslSYB on: " ++ show x


dsimple :: Data a => DSL -> Maybe a
dsimple = lift dinstance & lift dstring & lift dexp 


dinstance :: DSL -> Maybe Decl
dinstance x = do
    Instance _ name bod <- return x
    bod <- syb bod
    return $ InstDecl sl
        [ClassA (UnQual $ Ident "Data") [TyVar $ Ident "a"]]
        (UnQual $ Ident name) [TyVar $ Ident "a"]
        bod


dstring :: DSL -> Maybe String
dstring x = do
    String x <- return x
    return x


dexp :: DSL -> Maybe [Exp]
dexp x = return $ [Var $ UnQual $ Ident "TODO"]
