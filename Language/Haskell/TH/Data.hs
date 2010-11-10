
-- | The core module of the Data.Derive system.  This module contains
-- the data types used for communication between the extractors and
-- the derivors.
module Language.Haskell.TH.Data where

import Data.Char
import Data.Generics

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Compat


-- must be one of DataD or NewtypeD
type DataDef = Dec

type CtorDef = Con


dataName :: DataDef -> String
dataName (DataD    _ name _ _ _) = unqualifiedName name
dataName (NewtypeD _ name _ _ _) = unqualifiedName name

qualifiedDataName :: DataDef -> Name
qualifiedDataName (DataD    _ name _ _ _) = name
qualifiedDataName (NewtypeD _ name _ _ _) = name

dataArity :: DataDef -> Int
dataArity (DataD    _ _ xs _ _) = length xs
dataArity (NewtypeD _ _ xs _ _) = length xs

dataArgs :: DataDef -> [Name]
dataArgs = dataDefinitionTypeArgs


dataCtors :: DataDef -> [CtorDef]
dataCtors (DataD    _ _ _ xs _) = xs
dataCtors (NewtypeD _ _ _ x  _) = [x]


ctorName :: CtorDef -> String
ctorName (NormalC name _ ) = unqualifiedName name
ctorName (RecC name _    ) = unqualifiedName name
ctorName (InfixC _ name _) = unqualifiedName name
ctorName (ForallC _ _ c  ) = ctorName c

qualifiedCtorName :: CtorDef -> Name
qualifiedCtorName (NormalC name _ ) = name
qualifiedCtorName (RecC name _    ) = name
qualifiedCtorName (InfixC _ name _) = name
qualifiedCtorName (ForallC _ _ c  ) = qualifiedCtorName c


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
ctorFields (RecC name varStrictType) = [unqualifiedName name | (name,strict,typ) <- varStrictType]
ctorFields _ = []


-- normalisation

-- make sure you deal with "GHC.Base.."
dropModule :: String -> String
dropModule xs = case reverse xs of
                    ('.':xs) -> takeWhile (== '.') xs
                    xs -> reverse $ takeWhile (/= '.') xs

-- i_123432 -> i
dropNumber :: String -> String
dropNumber xs = if all isDigit a then reverse (tail b) else xs
    where (a,b) = break (== '_') $ reverse xs


normData :: DataDef -> DataDef
normData = everywhere (mkT normType)
    where
        normType :: Type -> Type
        normType (ConT x) | show x == "[]" = ListT
        normType x = x

unqualifiedName :: Name -> String
unqualifiedName = dropModule . show


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
    where sx = nameBase x
isTupleT _ = False
