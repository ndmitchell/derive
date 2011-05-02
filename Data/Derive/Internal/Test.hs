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

data PolyRecord i = PR { pr1 :: i,
                         pr2 :: [i] }

data Sample a = First | Second a a | Third a

data a :*: b = a :*: b

data Assoced typ = Assoced typ String

data FailList e a = Zoro | Fial e | Const a (FailList e a)

data State s a = StateT (s -> (s, a))

data TwoParam a b = TwoParam b
-}
