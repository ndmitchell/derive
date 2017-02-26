
module Data.Derive.Internal.Derivation(
    Derivation(..),
    derivationParams, derivationCustom, derivationDSL, derivationCustomDSL,
    customSplice, customContext
    ) where

import Data.DeriveDSL
import Data.Derive.DSL.HSE
import Data.Generics.Uniplate.DataOnly


data Derivation = Derivation
    {derivationName :: String
    ,derivationOp :: Type () -> (String -> Decl ()) -> FullDataDecl -> Either String [Decl ()]
    }


derivationParams :: String -> ([Type ()] -> (String -> Decl ()) -> FullDataDecl -> Either String [Decl ()]) -> Derivation
derivationParams name op = Derivation name $ \ty grab decs -> op (snd $ fromTyApps $ fromTyParen ty) grab decs


derivationCustom :: String -> (FullDataDecl -> Either String [Decl ()]) -> Derivation
derivationCustom name op = derivationParams name $ \ty grab decs -> op decs


derivationDSL :: String -> DSL -> Derivation
derivationDSL name dsl = derivationCustomDSL name (const id) dsl


derivationCustomDSL :: String -> (FullDataDecl -> [Decl ()] -> [Decl ()]) -> DSL -> Derivation
derivationCustomDSL name custom dsl = derivationCustom name $
    \d -> case applyDSL dsl $ snd d of
              Left x -> Left x
              Right x -> Right $ simplify $ custom d x


customSplice :: (FullDataDecl -> Exp () -> Exp ()) -> (FullDataDecl -> [Decl ()] -> [Decl ()])
customSplice custom d = transformBi f
    where
        f (SpliceExp () (ParenSplice () x)) = custom d x
        f x = x


customContext :: (FullDataDecl -> Context () -> Context ()) -> (FullDataDecl -> [Decl ()] -> [Decl ()])
customContext custom ds = transformBi (custom ds)
