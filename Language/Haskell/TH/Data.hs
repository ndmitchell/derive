-- | The core module of the Data.Derive system.  This module contains
-- the data types used for communication between the extractors and
-- the derivors.
module Language.Haskell.TH.Data where

import Data.List
import Data.Char

import Language.Haskell.TH.Syntax


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



