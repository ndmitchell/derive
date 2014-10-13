
module Data.Derive.Class.Arities where

class Arities a where
    arities :: a -> [Int]
