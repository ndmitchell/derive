{-# LANGUAGE PatternGuards #-}

-- | Expand type synonyms in data declarations.
-- 
--   This is needed for some type based derivations.
module Language.Haskell.TH.ExpandSynonym (expandData) where

import Language.Haskell.TH
import Language.Haskell.TH.Compat
import Language.Haskell.TH.Data
import Data.Generics

-- | Expand type synonyms in a data declaration
expandData :: DataDef -> Q DataDef
expandData = everywhereM (mkM expandType)

expandType :: Type -> Q Type
expandType t = expandType' t []

-- Walk over a type, collecting applied arguments
expandType' :: Type -> [Type] -> Q Type
expandType'   (AppT t arg) args   = expandType' t (arg:args)
expandType' t@(ConT name)  args   = do result <- expandSyn name args
                                       case result of
                                          Just (t',args') -> everywhereM (mkM expandType) $ foldl AppT t' args'
                                          _               -> return $ foldl AppT t args
expandType' t              args   = return $ foldl AppT t args

-- Is the name a type synonym and are there enough arguments? if so, apply it
expandSyn :: Name -> [Type] -> Q (Maybe (Type, [Type]))
expandSyn name args = recover (return Nothing) $ do
            info <- reify name
            case info of
                   TyConI (TySynD _ synArgs t) | length args >= length synArgs
                        -> return $ Just (substitute (map fromTyVar synArgs) argsInst t, argsMore) -- instantiate type synonym
                             where (argsInst,argsMore) = splitAt (length synArgs) args
                   _    -> return Nothing
      -- `recover` return Nothing

-- Substitute names for types in a type
substitute :: [Name] -> [Type] -> Type -> Type
substitute ns ts = subst (zip ns ts)
  where subst s (ForallT ns ctx t) = ForallT ns ctx (subst (filter ((`notElem` (map fromTyVar ns)) . fst) s) t)
        subst s (VarT n)
           | Just t' <- lookup n s = t'
        subst s (AppT a b)         = AppT (subst s a) (subst s b)
        subst _ t                  = t
