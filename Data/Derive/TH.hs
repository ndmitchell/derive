{-# OPTIONS_GHC -fglasgow-exts #-}
-- | The main TH driver module.  It is intended that this need be the
-- only module imported by user code; it takes care of all data
-- threading issues such that all one needs to do is:
--
-- @
--   data Foo = Foo ; $( derive 'Data.Derive.StdDerivations.eq' ''Foo )
-- @
module Data.Derive.TH
       (derive,
        -- * Convienience re-exports
        Derivation, -- abstract!
       ) where

import Data.Derive
import Data.List
import Data.Maybe

import Language.Haskell.TH.Syntax
import Data.Derive.FixedPpr

-- | Derive an instance of some class.  This uses the Scrap Your
-- Boilerplate infrastructure to extract the data type definition; to
-- resolve overloading the second argument to @derive@ is a phantom
-- value of the type you wish the instance to be derived for.  The
-- first argument is the class name.  @derive@ only derives instances
-- for the type of the argument; to derive instances for an entire
-- dependency group of data types, use 'derives'.
derive :: Derivation -> Name -> Q [Dec]
derive (Derivation f _) = fmap f . deriveOne

-- | Extract a 'DataDef' value from a type using the TH 'reify'
-- framework.
deriveOne :: Name -> Q DataDef
deriveOne x = fmap extract (reify x)

extract (TyConI decl) = extract' decl
extract _ = error $ "Data.Derive.TH.deriveInternal: not a type!"

extract' (DataD _cx name args cons _derv)
    = DataDef (show name) (length args) (map (ex_ctor args) cons)
extract' (NewtypeD cx name args con derv) = extract' (DataD cx name args [con] derv)
extract' _ = error "strange tycon decl!"

ctord n ty = CtorDef (show n) (length ty) ty

ex_ctor :: [Name] -> Con -> CtorDef
ex_ctor args (NormalC nm tys) = ctord nm   $ map (ex_type args . snd) tys
ex_ctor args (RecC name tys)  = ctord name $ map (ex_type args . (\ (x,y,z) -> z)) tys
ex_ctor args (InfixC t0 n t1) = ctord n    $ map (ex_type args . snd) [t0, t1]
ex_ctor args ForallC{}        = error "Existential types not yet handled"

ex_type :: [Name] -> Type -> RType
ex_type args ForallT{}  = error "Polymorphic components not supported"
ex_type args (VarT nm)  = case elemIndex nm args of
                                Nothing -> error "impossible: tyvar not in scope"
                                Just k  -> RType (TypeArg k) []
ex_type args (ConT nm)  = RType (TypeCon (show nm)) []
ex_type args (TupleT k) = RType (TypeCon ("(" ++ replicate (k-1) ',' ++ ")")) []
ex_type args (ArrowT)   = RType (TypeCon "(->)") []
ex_type args (ListT)    = RType (TypeCon "[]") []
ex_type args (AppT a b) = let (RType tc ar) = ex_type args a ; arg = ex_type args b
                            in RType tc (ar ++ [arg])
