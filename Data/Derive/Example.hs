module Data.Derive.Example(Data2(..), is, guess) where

import Language.Haskell.TH


data Data2 a b = Ctor0
               | Ctor1 a
               | Ctor2 a b

is :: a -> a
is x = x


guess :: Q [Dec] -> IO ()
guess x = error "Failed to guess (guessing not yet implemented)"

