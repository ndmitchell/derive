-- | This module provides convenience re-exports of all the standard
-- Data.Derive derivations.
module Data.Derive.All (Derivation, derivations, module D) where

import Data.Derive.Internal.Derivation

-- GENERATED START
import Data.Derive.Arbitrary        as D
import Data.Derive.ArbitraryOld     as D
import Data.Derive.Arities          as D
import Data.Derive.Binary           as D
import Data.Derive.BinaryDefer      as D
import Data.Derive.Bounded          as D
import Data.Derive.Data             as D
import Data.Derive.Default          as D
import Data.Derive.Enum             as D
import Data.Derive.EnumCyclic       as D
import Data.Derive.Eq               as D
import Data.Derive.Fold             as D
import Data.Derive.Foldable         as D
import Data.Derive.From             as D
import Data.Derive.Functor          as D
import Data.Derive.Has              as D
import Data.Derive.Is               as D
import Data.Derive.LazySet          as D
import Data.Derive.Monoid           as D
import Data.Derive.NFData           as D
import Data.Derive.Ord              as D
import Data.Derive.PlateDirect      as D
import Data.Derive.PlateTypeable    as D
import Data.Derive.Read             as D
import Data.Derive.Ref              as D
import Data.Derive.Serial           as D
import Data.Derive.Set              as D
import Data.Derive.Show             as D
import Data.Derive.Traversable      as D
import Data.Derive.TTypeable        as D
import Data.Derive.Typeable         as D
import Data.Derive.Uniplate         as D
import Data.Derive.Update           as D
derivations :: [Derivation]
derivations = [makeArbitrary,makeArbitraryOld,makeArities,makeBinary,makeBinaryDefer,makeBounded,makeData,makeDefault,makeEnum,makeEnumCyclic,makeEq,makeFold,makeFoldable,makeFrom,makeFunctor,makeHas,makeIs,makeLazySet,makeMonoid,makeNFData,makeOrd,makePlateTypeable,makeRef,makeSerial,makeSet,makeShow,makeTypeable,makeUpdate]
-- GENERATED STOP
