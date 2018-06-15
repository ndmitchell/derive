{-# LANGUAGE DeriveDataTypeable #-}

module Data.Derive.DSL.HSE(module Data.Derive.DSL.HSE, module Language.Haskell) where

import Language.Haskell hiding (List, App, String, Int)
import Data.Data
import Data.Generics.Uniplate.DataOnly
import Data.Maybe
import Data.List
import Data.Function
import Control.Monad.Trans.State


---------------------------------------------------------------------
-- EXAMPLES

{-
-- data List a = Nil | Cons a (List a)
list :: Input
list = Input "List" 1 [Ctor "Nil" 0 0, Ctor "Cons" 1 2]
-}


-- data Sample a = First | Second a a | Third a
sample :: Input
sample = DataDecl () (DataType ()) Nothing (DHApp () (DHead () $ name "Sample") (tyVarBind "a")) ctrs []
    where
        ctrs = [ctr "First" 0, ctr "Second" 2, ctr "Third" 1]
        ctr s i = QualConDecl () Nothing Nothing $ ConDecl () (name s) $ replicate i $ tyVar "a"


---------------------------------------------------------------------
-- UTILITIES

outEq :: Out -> Out -> Bool
outEq = (==) `on` transformBi (const sl)

---------------------------------------------------------------------

showOut x = unlines $ map prettyPrint x


type Input = DataDecl
type Ctor = CtorDecl

dataName = dataDeclName
dataVars = length . dataDeclVars
dataCtors = dataDeclCtors
ctorName = ctorDeclName
ctorArity = fromIntegral . ctorDeclArity

ctorIndex :: Input -> Ctor -> Integer
ctorIndex dat ctor = fromIntegral $ fromMaybe (error "fromJust: ctorIndex") $ findIndex (== ctor) $ dataCtors dat


toInput :: DataDecl -> Input
toInput x = x


type Out = [Decl ()]



data Output = OString String
            | OInt Integer
            | OApp String [Output]
            | OList [Output]
            | OIgnore
            | OCustom String
              deriving (Eq,Show,Data,Typeable)


toOutput :: Data a => a -> Output
toOutput x
    | t == typeOf "" = OString $ coerce x
    | c == "[]" = OList $ fList x
    | t == typeOf sl = OIgnore
    | t == typeOf (1 :: Integer) = OInt $ coerce x
    | otherwise = OApp (showConstr $ toConstr x) (filter (/= OIgnore) $ gmapQ toOutput x)
    where
        t = typeOf x
        c = show $ fst $ splitTyConApp t

        fList :: Data a => a -> [Output]
        fList = gmapQl (++) [] $ \x -> if typeOf x == t then fList x else [toOutput x]

fromOutput :: Data a => Output -> a
fromOutput (OList xs) = res
    where res = f xs
          f [] = fromConstr $ readCon dat "[]"
          f (x:xs) = fromConstrB (g x (f xs `asTypeOf` res)) $ readCon dat "(:)"
          dat = dataTypeOf res

          g :: (Data a, Data b) => Output -> a -> b
          g x xs = r2 where r2 = if typeOf r2 == typeOf xs then coerce xs else fromOutput x

fromOutput (OApp str args) = res
    where dat = dataTypeOf res
          res = evalState (fromConstrM f $ readCon dat str) args
          f :: Data a => State [Output] a
          f = res where res = if typeOf (fromState res) == typeOf sl then return $ coerce sl else
                              do x:xs <- get; put xs; return $ fromOutput x

fromOutput (OString x) = coerce x
fromOutput (OInt x) = coerce x


coerce x = fromMaybe (error "Error in coerce") $ cast x
readCon dat x = fromMaybe (error $ "Error in readCon, " ++ x) $ readConstr dat x
out x = toOutput x
fromState :: State a x -> x
fromState = undefined
