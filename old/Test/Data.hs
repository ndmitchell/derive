{-# LANGUAGE TemplateHaskell #-}

module Test.Data(
    module Test.Data, module Data.DeriveTH
    ) where

import Control.Monad
import Data.DeriveTH
import Data.List
import Language.Haskell.TH.Syntax


concatMapM f = liftM concat . mapM f


-- | Test that the given derivations work for all 'names'
derivess :: [Derivation] -> Q [Dec]
derivess = concatMapM derives


-- | Test that the given derivation workds for all names, except for some bad ones
derivesNot :: Derivation -> [Name] -> Q [Dec]
derivesNot f bad = concatMapM (derive f) (names \\ bad)


-- | Test that the given derivation works for all 'names'
derives :: Derivation -> Q [Dec]
derives f = concatMapM (derive f) names


names :: [Name]
names = [''A, ''B, ''Color, ''Computer, ''Drinks, ''FailList, ''State, ''Tree, ''Tree2, ''Tree3]


data A a = A a [C a]
data B a = B a
type C a = B (B a)


data Foo = Bar


data Color = RGB Int Int Int
           | CMYK Int Int Int Int


data Computer = Laptop { weight :: Int }
              | Desktop { speed :: Int, memory :: Int }


-- | All drinks mankind will ever need
data Drinks = Beer Bool{-ale?-}
            | Coffee
            | Tea
            | EnergyDrink
            | Water
            | Wine
            | Whisky


-- | A list with late failure
data FailList e a = Nil | Fail e | Const a (FailList e a)


-- | State monad
data State s a = StateT (s -> (s, a))


-- | A rose tree
data Tree a = Branch a [Tree a]

-- | A binary tree
data Tree2 a
    = Empty
    | Node (Tree2 a) a (Tree2 a)

-- | A balanced binary tree
data Tree3 a
    = Done a
    | Deep (Tree3 (a,a))

