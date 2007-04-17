-- | The core module of the Data.Derive system.  This module contains
-- the data types used for communication between the extractors and
-- the derivors.
module Language.Haskell.TH.Data where

import Data.List
import Data.Char
import Data.Generics

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.SYB


-- must be one of DataD or NewtypeD
type DataDef = Dec

type CtorDef = Con


dataName :: DataDef -> String
dataName (DataD    _ name _ _ _) = show name
dataName (NewtypeD _ name _ _ _) = show name


dataArity :: DataDef -> Int
dataArity (DataD    _ _ xs _ _) = length xs
dataArity (NewtypeD _ _ xs _ _) = length xs


dataCtors :: DataDef -> [CtorDef]
dataCtors (DataD    _ _ _ xs _) = xs
dataCtors (NewtypeD _ _ _ x  _) = [x]


ctorName :: CtorDef -> String
ctorName (NormalC name _ ) = show name
ctorName (RecC name _    ) = show name
ctorName (InfixC _ name _) = show name
ctorName (ForallC _ _ c  ) = ctorName c


ctorArity :: CtorDef -> Int
ctorArity (NormalC _ xs ) = length xs
ctorArity (RecC _ xs    ) = length xs
ctorArity (InfixC _ _ _ ) = 2
ctorArity (ForallC _ _ c) = ctorArity c


ctorStrictTypes :: CtorDef -> [StrictType]
ctorStrictTypes (NormalC _ xs ) = xs
ctorStrictTypes (RecC _ xs    ) = [(b,c) | (a,b,c) <- xs]
ctorStrictTypes (InfixC x _ y ) = [x,y]
ctorStrictTypes (ForallC _ _ c) = ctorStrictTypes c


ctorTypes :: CtorDef -> [Type]
ctorTypes = map snd . ctorStrictTypes


ctorFields :: CtorDef -> [String]
ctorFields (RecC name xs) = [show a | (a,b,c) <- xs]
ctorFields _ = []


-- normalisation

normData :: DataDef -> DataDef
normData = everywhere (mkT normType) . everywhere (mkT normName)
    where
        normName :: Name -> Name
        normName = mkName . reverse . takeWhile (/= '.') . reverse . show

        normType :: Type -> Type
        normType (ConT x) | show x == "[]" = ListT
        normType x = x



-- convert AppT chains back to a proper list
typeApp :: Type -> (Type, [Type])
typeApp (AppT l r) = (a, b++[r])
    where (a,b) = typeApp l
typeApp t = (t, [])



eqConT :: String -> Type -> Bool
eqConT name (ConT x) = name == show x
eqConT _ _ = False

isTupleT :: Type -> Bool
isTupleT (TupleT _) = True
isTupleT (ConT x) = head sx == '(' && last sx == ')' &&
                    all (== ',') (take (length sx - 2) (tail sx))
    where sx = show x
isTupleT _ = False




-- * Depreciated, old type stuff

data RType    = RType {typeCon :: TypeCon, typeArgs :: [RType] }
    deriving (Eq, Ord)

-- | A referencing type which is not itself an application.
data TypeCon = TypeCon String -- ^ A type defined elsewhere, free in
                              -- the data declaration.
             | TypeArg  Int   -- ^ A reference to a type bound by the
                              -- type constructor; the argument to
                              -- @TypeArg@ is the index of the type
                              -- argument, counting from zero at the
                              -- left.
    deriving (Eq, Ord)


instance Show RType where
    show (RType con [])   = show con
    show (RType con args) = "(" ++ show con ++ concatMap ((" "++) . show) args ++ ")"

instance Show TypeCon where
    show (TypeCon n) = n
    show (TypeArg i) = [chr (ord 'a' + i)]




ctorRTypes :: DataDef -> CtorDef -> [RType]
ctorRTypes dat (NormalC nm tys) = map (ex_type dat . snd) tys
ctorRTypes dat (RecC name tys)  = map (ex_type dat . (\ (x,y,z) -> z)) tys
ctorRTypes dat (InfixC t0 n t1) = map (ex_type dat . snd) [t0, t1]
ctorRTypes dat ForallC{}        = error "Existential types not yet handled"



ex_type :: DataDef -> Type -> RType
ex_type dat ForallT{}  = error "Polymorphic components not supported"
ex_type dat (VarT nm)  = case elemIndex nm (ex_args dat) of
                                Nothing -> error "impossible: tyvar not in scope"
                                Just k  -> RType (TypeArg k) []
ex_type dat (ConT nm)  = RType (TypeCon (show nm)) []
ex_type dat (TupleT k) = RType (TypeCon ("(" ++ replicate (k-1) ',' ++ ")")) []
ex_type dat (ArrowT)   = RType (TypeCon "(->)") []
ex_type dat (ListT)    = RType (TypeCon "[]") []
ex_type dat (AppT a b) = let (RType tc ar) = ex_type dat a ; arg = ex_type dat b
                         in RType tc (ar ++ [arg])


ex_args :: DataDef -> [Name]
ex_args (DataD _cx name args cons _derv) = args
ex_args (NewtypeD cx name args con derv) = args
