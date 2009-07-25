-- Not imported by anything, just a handy place to put test data types

module Data.Derive.Internal.Test() where

{-
data List a = Nil | Cons a (List a)

-- built in, as it can't be written
-- data [a] = [] | (:) a [a]

data Bool = False | True

data Either a b = Left a | Right b

data Computer = Laptop { weight :: Double, speed :: Int }
              | Desktop { speed :: Int }

data Sample a = First | Second a a | Third a

data a :*: b = a :*: b

data Assoced typ = Assoced typ String

data FailList e a = Zero | Fail e | Const a (FailList e a)

data State s a = StateT (s -> (s, a))

data TwoParam a b = TwoParam b


instance Show (a -> b)
instance Default (a -> b) where def = undefined
instance NFData (a -> b)
instance Ord (a -> b)
instance Bounded (a -> b) where minBound = undefined ; maxBound = undefined
instance Binary (a -> b) where put = undefined ; get = undefined
instance Eq (a -> b)
instance Read (a -> b)
-}
