{-# LANGUAGE RelaxedPolyRec, RankNTypes, ScopedTypeVariables #-}
{- OPTIONS_GHC -fglasgow-exts -}

module Data.Derive.DSL.SYB(dslSYB) where

import Data.Derive.DSL.HSE
import qualified Language.Haskell.Exts as H
import Data.Derive.DSL.DSL
import Control.Monad.Trans.State
import Control.Monad
import Data.Generics
import Data.Maybe


dslSYB :: DSL -> Maybe Out
dslSYB = syb


syb :: Data a => DSL -> Maybe a
syb = dsimple & dlistAny & dapp -- & (\x -> error $ "Failed to generate for SYB, " ++ show x)

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


dsimple :: Data a => DSL -> Maybe a
dsimple = lift dinstance & lift dstring & lift dapplication & lift dmapctor & lift dsingle


dinstance :: DSL -> Maybe (Decl ())
dinstance x = do
    Instance _ name bod <- return x
    bod <- syb bod
    let ctx = ClassA () (UnQual () $ Ident () "Data") [TyVar () $ Ident () "d_type"]
    let rule = IRule () Nothing (Just (CxSingle () ctx))
                (IHApp () (IHCon () (UnQual () $ Ident () name)) (TyVar () $ Ident () "d_type"))
    return $ InstDecl () Nothing rule bod


dstring :: DSL -> Maybe String
dstring x = do
    String x <- return x
    return x


dmapctor :: DSL -> Maybe (Exp ())
dmapctor x = do
    App "List" (List [_, MapCtor x]) <- return x
    x <- syb x
    return $ ListComp () x [QualStmt () $ Generator () (PVar () $ Ident () "d_ctor")
        (H.App () (v "d_dataCtors") (Paren () $ ExpTypeSig () (v "undefined") (TyVar () $ Ident () "d_type")))]


dsingle :: DSL -> Maybe (Exp ())
dsingle (App "Lit" (List [App "()" (List []),App "Int" (List [App "()" (List []),CtorArity,ShowInt CtorArity])])) = Just $ Paren () $ H.App () (v "d_ctorArity") (v "d_ctor")
dsingle (App "Lit" (List [App "()" (List []),App "Int" (List [App "()" (List []),CtorIndex,ShowInt CtorIndex])])) = Just $ Paren () $ H.App () (v "d_ctorIndex") (v "d_ctor")
dsingle (App "RecConstr" (List [_, App "UnQual" (List [_, App "Ident" (List [_, CtorName])]),List []])) = Just $ Paren () $
    ExpTypeSig () (H.App () (v "d_ctorValue") (v "d_ctor")) (TyVar () $ Ident () "d_type")
dsingle _ = Nothing


dapplication :: DSL -> Maybe (Exp ())
dapplication x = do
    Application (List xs) <- return x
    syb $ f xs
    where
        f (x:y:z) = f (App "App" (List [App "()" $ List [],x,y]) : z)
        f [x] = x


v = Var () . UnQual () . Ident ()
