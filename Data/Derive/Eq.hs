{-# OPTIONS_GHC -fth #-}
module Data.Derive.Eq(eq) where

import Data.Derive
import Data.List
import Language.Haskell.TH

eq = Derivation eq' "Eq"
eq' dat@(DataDef _ _ ctors) = simple_instance ''Eq dat [FunD '(==) (cases ++ [end])]
    where
        cases = map (\ctor -> rule (ctorName ctor) (ctorArity ctor)) ctors
        end = Clause [WildP, WildP] (NormalB $ ConE 'False) []

rule nam arity = Clause pats (NormalB $ if arity == 0 then ConE 'True else body) []
    where
        na f c = map (f . mkName . (c:) . show) [1..arity]
        pats = (map (\c -> ConP (mkName nam) (na VarP c)) "ab")
        infx op a b = InfixE (Just a) (VarE op) (Just b)
        body = foldr1 (infx '(&&)) (zipWith (infx '(==)) (na VarE 'a') (na VarE 'b'))

simple_instance cls (DataDef name arity _) defs = [InstanceD ctx hed defs]
    where
        vars = map (VarT . mkName . ('t':) . show) [1..arity]
        hed = ConT cls `AppT` (foldl1 AppT (ConT (mkName name) : vars))
        ctx = map (ConT cls `AppT`) vars
