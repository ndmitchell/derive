{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

-- Would like to make the test suite warning free, but requires tweaks to the instances

module Test.Standard where

import Test.Data

import Data.Generics
import Data.Monoid


$( derivess [makeOrd, makeShow, makeEq, makeBounded, makeEnum, makeRead
            ,makeData, makeTypeable
            ,makeIs, makeHas, makeSet, makeFrom
            ,makeMonoid
            ] )


$( derivesNot makeFunctor [''State] )

instance Eq (a -> b)
instance Show (a -> b)
instance Ord (a -> b)
instance Bounded (a -> b)
instance Read (a -> b)

instance Bounded a => Bounded [a]

{-
makeArbitrary :: Language.Haskell.TH.All.Derivation
makeBinary :: Language.Haskell.TH.All.Derivation
makeBinaryDefer :: Language.Haskell.TH.All.Derivation
makeBinaryOld :: Language.Haskell.TH.All.Derivation
makeEnumCyclic :: Language.Haskell.TH.All.Derivation
makeFoldable :: Language.Haskell.TH.All.Derivation
makeNFData :: Language.Haskell.TH.All.Derivation
makePlateDirect :: Language.Haskell.TH.All.Derivation
makePlateTypeable :: Language.Haskell.TH.All.Derivation
makeRef :: Language.Haskell.TH.All.Derivation
makeLazySet
makeSerial :: Language.Haskell.TH.All.Derivation
makeTTypeable :: Language.Haskell.TH.All.Derivation
makeTraversable :: Language.Haskell.TH.All.Derivation
makeUniplate :: Language.Haskell.TH.All.Derivation
-}
