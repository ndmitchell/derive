
module QuickCheck where

data Expr name = ELambda name (Expr name)
               | EAp (Expr name) (Expr name)
               | EVar name
                 deriving ({-! Eq, Ord, Show, Arbitrary !-})

data Primary = Red | Green | Blue
               deriving ({-! Eq, Ord, Show, Arbitrary !-})

newtype Id a = Id a deriving ({-! Eq, Ord, Show, Arbitrary !-})
