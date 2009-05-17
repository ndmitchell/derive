{-# LANGUAGE DeriveDataTypeable #-}

module HSE(module HSE, module Language.Haskell.Exts) where

import Language.Haskell.Exts hiding (List, App, String, Int)
import qualified Language.Haskell.Exts as H
import Data.Data
import Data.Generics.PlateData
import Data.Maybe
import Data.List
import Unsafe.Coerce
import Control.Monad.State


---------------------------------------------------------------------
-- EXAMPLES

sl = SrcLoc "" 0 0

-- data List a = Nil | Cons a (List a)
list :: Input
list = Input "List" 1 [Ctor "Nil" 0 0, Ctor "Cons" 1 2]


-- data Sample a = First | Second a a | Third a
sample :: Input
sample = Input "Sample" 1 [Ctor "First" 0 0, Ctor "Second" 1 2, Ctor "Third" 2 1]


---------------------------------------------------------------------
-- UTILITIES

isUnknownDeclPragma UnknownDeclPragma{} = True
isUnknownDeclPragma _ = False


simplifyOut :: Out -> Out
simplifyOut = transformBi fExp
    where
        x ~= y = prettyPrint x == y
    
        fExp (H.App op (H.List xs))
            | op ~= "length" = Lit $ H.Int $ fromIntegral $ length xs
            | op ~= "head" = head xs
        fExp (InfixApp (Lit (H.Int i)) op (Lit (H.Int j)))
            | op ~= "-" = Lit $ H.Int $ i - j
            | op ~= ">" = Con $ UnQual $ Ident $ show $ i > j
        fExp (InfixApp x op y) | op ~= "`const`" = x
        fExp (H.App (H.App con x) y) | con ~= "const" = x
        fExp (Paren (Var x)) = Var x
        fExp (Paren (Lit x)) = Lit x
        fExp x = x


---------------------------------------------------------------------

showOut x = unlines $ map prettyPrint x


data Input = Input {dataName :: String, dataVars :: Integer, dataCtors :: [Ctor]}
data Ctor = Ctor {ctorName :: String, ctorIndex :: Integer, ctorArity :: Integer}


dataTypeInput :: Decl -> Input
dataTypeInput (DataDecl _ _ _ name vars ctors _) = Input (prettyPrint name) (genericLength vars) (zipWith f [0..] ctors)
    where f index (QualConDecl _ _ _ (ConDecl name fields)) = Ctor (prettyPrint name) index (genericLength fields)


type Out = [Decl]



data Output = OString String
            | OInt Integer
            | OApp String [Output]
            | OList [Output]
            | OIgnore
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
          typ = typeOf res
          
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
