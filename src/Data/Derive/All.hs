-- | This module provides convenience re-exports of all the standard
-- Data.Derive derivations.
module Data.Derive.All (Derivation, derivations, module D) where

import Data.Derive.Internal.Derivation

-- GENERATED START
import Data.Derive.Arbitrary           as D
import Data.Derive.ArbitraryOld        as D
import Data.Derive.Arities             as D
import Data.Derive.Binary              as D
import Data.Derive.BinaryDefer         as D
import Data.Derive.Bounded             as D
import Data.Derive.DataAbstract        as D
import Data.Derive.Default             as D
import Data.Derive.EnumCyclic          as D
import Data.Derive.Fold                as D
import Data.Derive.From                as D
import Data.Derive.Has                 as D
import Data.Derive.Is                  as D
import Data.Derive.JSON                as D
import Data.Derive.LazySet             as D
import Data.Derive.Lens                as D
import Data.Derive.Monoid              as D
import Data.Derive.NFData              as D
import Data.Derive.Ref                 as D
import Data.Derive.Serial              as D
import Data.Derive.Serialize           as D
import Data.Derive.Set                 as D
import Data.Derive.UniplateDirect      as D
import Data.Derive.UniplateTypeable    as D
import Data.Derive.Update              as D
derivations :: [Derivation]
derivations = [makeArbitrary,makeArbitraryOld,makeArities,makeBinary,makeBinaryDefer,makeBounded,makeDataAbstract,makeDefault,makeEnumCyclic,makeFold,makeFrom,makeHas,makeIs,makeJSON,makeLazySet,makeLens,makeMonoid,makeNFData,makeRef,makeSerial,makeSerialize,makeSet,makeUniplateDirect,makeUniplateTypeable,makeUpdate]
-- GENERATED STOP
