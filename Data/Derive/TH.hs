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
        -- * Internal
        _derive_print_instance
       ) where

import Data.Derive
import Data.List
import Control.Monad (liftM)

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.FixedPpr

-- | Derive an instance of some class. @derive@ only derives instances
-- for the type of the argument.
derive :: Derivation -> Name -> Q [Dec]
derive (Derivation f _) = liftM f . deriveOne

-- | Derive for a type and print the code to standard output.  This is
-- a internal hook for the use of the Derive executable.
_derive_print_instance :: Derivation -> Name -> Q Exp
_derive_print_instance (Derivation f _) nm =
    return . l1 "putStr" . LitE . StringL . (++"\n\n") . show . ppr . f =<< deriveOne nm

-- | Extract a 'DataDef' value from a type using the TH 'reify'
-- framework.
deriveOne :: Name -> Q DataDef
deriveOne x = liftM extract (reify x)

extract (TyConI decl) = extract' decl
extract _ = error $ "Data.Derive.TH.deriveInternal: not a type!"

extract' (DataD _cx name args cons _derv)
    = DataDef (show name) (length args) (map (ex_ctor args) cons)
extract' (NewtypeD cx name args con derv) = extract' (DataD cx name args [con] derv)
extract' _ = error "strange tycon decl!"

ctord n ty = CtorDef (show n) ty

ex_ctor :: [Name] -> Con -> CtorDef
ex_ctor args (NormalC nm tys) = ctord nm   . zip (repeat Nothing) . map (ex_type args . snd) $ tys
ex_ctor args (RecC name tys)  = ctord name $ map (\ (x,y,z) -> (Just $ show x , ex_type args z)) tys
ex_ctor args (InfixC t0 n t1) = undefined --ctord n    - zip (repeat Nothing) . map (ex_type args . snd) $ [t0, t1]
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
