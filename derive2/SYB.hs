{-# LANGUAGE RelaxedPolyRec, RankNTypes, ScopedTypeVariables #-}
{- OPTIONS_GHC -fglasgow-exts -}

module SYB(dslSYB) where

import HSE
import DSL
import Control.Monad
import Control.Monad.State
import Data.Generics
import Data.Maybe


dslSYB :: DSL -> Maybe Out
dslSYB = syb


syb :: forall a . Data a => DSL -> Maybe a
syb = f0 dlistAny + f dinst + f dstring + f0 dapp + f dexp + f0 derr
    where
        f0 :: (forall b . Data b => DSL -> Maybe b) -> (DSL -> Maybe a)
        f0 = id

        f :: Data b => (DSL -> Maybe b) -> (DSL -> Maybe a)
        f g x = maybe Nothing id $ cast $ g x

        (+) a b x = a x `mplus` b x


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


dinst :: DSL -> Maybe Decl
dinst x = do
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


dexp :: DSL -> Maybe [Exp]
dexp x = return $ [Var $ UnQual $ Ident "TODO"]

{-




sybt x `mplus` f x
    where
        f (App name (List args)) = if fst s then Just res else Nothing
            where
                (s,res) = runState (fromConstrM f $ readCon dat name) (True,args)
                dat = dataTypeOf res
                f :: Data a => State (Bool,[DSL]) a
                f = res
                  where
                   res =
                    if typeOf (fromState res) == typeOf sl then return $ coerce sl
                    else do
                        (b,x:xs) <- get
                        case syb x of
                            Nothing -> do put (False,xs) ; return undefined
                            Just y -> do put (b,xs) ; return y
        f _ = Nothing


    inst 


class Data a => SYB a where
    sybt :: DSL -> Maybe a
    sybt = const Nothing


instance SYB a => SYB [a] where
    sybt x = do
        List xs <- return x
        mapM syb xs

instance SYB Decl where
    sybt x = do
        Instance _ name bod <- return x
        bod <- syb bod
        return $ InstDecl sl
            [ClassA (UnQual $ Ident "Data") [TyVar $ Ident "a"]]
            (UnQual $ Ident name) [TyVar $ Ident "a"]
            bod


instance SYB InstDecl
instance SYB Match
instance SYB Rhs
instance SYB Type
instance SYB Binds
instance SYB Pat
instance SYB Name

-}
