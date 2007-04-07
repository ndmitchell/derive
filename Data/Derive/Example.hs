module Data.Derive.Example(Data2(..), guess) where

import Language.Haskell.TH


data Data2 a b = Ctor0
               | Ctor1 a
               | Ctor2 a b


guess :: Q [Dec] -> IO ()
guess x = error "Failed to guess (guessing not yet implemented)"

