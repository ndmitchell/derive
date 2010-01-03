{-# LANGUAGE CPP #-}

-- | Compatibility definitions to paper over differences between 6.10 and 6.12.
module Language.Haskell.TH.Compat where

import Language.Haskell.TH


#if __GLASGOW_HASKELL__ >= 612
fromTyVar :: TyVarBndr -> Name
fromTyVar (PlainTV v) = v
fromTyVar (KindedTV v _) = v
#else
fromTyVar :: Name -> Name
fromTyVar v = v
#endif


dataDefinitionTypeArgs :: Dec -> [Name]
#if __GLASGOW_HASKELL__ >= 612
dataDefinitionTypeArgs (DataD _cx name _ _ args) = args
dataDefinitionTypeArgs (NewtypeD cx name _ _ args) = args
#else
dataDefinitionTypeArgs (DataD _cx name args cons _derv) = args
dataDefinitionTypeArgs (NewtypeD cx name args con derv) = args
#endif


#if __GLASGOW_HASKELL__ >= 612
typeToPred :: Type -> Pred
typeToPred (ConT v) = ClassP v []
typeToPred (AppT x y) = ClassP v (t++[y])
    where ClassP v t = typeToPred x
#else
typeToPred :: Type -> Type
typeToPred x = x
#endif
