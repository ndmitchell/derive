module Small where

import Data.Generics


data Expr name = ELambda name (Expr name)
               | EAp (Expr name) (Expr name)
               | EVar name
                 deriving ({-! Eq, Ord, Read, Show, Typeable, Data, From, Is !-})


data Primary = Red | Green | Blue
               deriving ({-! Eq, Ord, Enum, Bounded, Read, Show,
                             Typeable, Data, From, Is !-})


data BinTree a = Leaf | Branch a (BinTree a) (BinTree a)
                 deriving ({-! Eq, Ord, Read, Show, Typeable, Data, From !-})


-- This doesn't work :-(

-- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
--                      deriving ({-! Eq, Ord, Read, Show, Typeable, Data, Monoid !-})

newtype Id a = Id a deriving ({-! Eq, Ord, Enum, Bounded, Read, Show, Typeable, Data, Functor, Monoid, From !-})

newtype Id2 a = Id2 { runId :: a }
    deriving ({-! Eq, Ord, Enum, Bounded, Read, Show, Typeable, Data, Functor, Monoid, Set, LazySet, From !-})
