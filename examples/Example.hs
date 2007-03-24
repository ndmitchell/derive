{-# OPTIONS_GHC -fglasgow-exts #-}

module Example where

import Data.Generics


data Foo = Bar
    deriving (Typeable, Data, Show, Eq)


data Color = RGB Int Int Int
           | CMYK Int Int Int Int
    deriving (Typeable, Data, Show, Eq)


data Computer = Laptop { weight :: Int }
              | Desktop { speed :: Int, memory :: Int }
    deriving (Typeable, Data, Show, Eq)


-- | All drinks mankind will ever need
data Drinks = Beer Bool{-ale?-}
            | Coffee
            | Tea
            | EnergyDrink
            | Water
            | Wine
            | Whisky
    deriving (Typeable, Data, Show, Eq)

-- | A list with late failure
data FailList e a = Nil | Fail e | Const a (FailList e a)
    deriving (Typeable, Data, Show, Eq)

-- | State monad
data State s a = StateT (s -> (s, a))
    deriving (Typeable, Data)
