module Small where

import Data.Generics


data Expr name = ELambda name (Expr name)
               | EAp (Expr name) (Expr name)
               | EVar name
                 deriving ({-! Eq, Ord, Read, Show, Typeable, Data !-})


data Primary = Red | Green | Blue
               deriving ({-! Eq, Ord, Enum, Bounded, Read, Show,
                             Typeable, Data !-})


data BinTree a = Leaf | Branch a (BinTree a) (BinTree a)
                 deriving ({-! Eq, Ord, Read, Show, Typeable, Data !-})


-- This doesn't work :-(

-- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
--                      deriving ({-! Eq, Ord, Read, Show, Typeable, Data, Monoid !-})


newtype Id a = Id a deriving ({-! Eq, Ord, Enum, Bounded, Read, Show, Typeable, Data, Functor, Monoid !-})
