{-# LANGUAGE CPP, DeriveDataTypeable #-}
module M2 where
#ifdef GHC_DERIVING
import Data.Generics
#endif

data Exp = Null | One (Id Int) | Two (Id2 Bool) Exp
#ifdef GHC_DERIVING
            deriving (Eq,Ord,Read,Show,Typeable,Data)
#else
            deriving ({-!  Eq, Ord, Enum, Bounded, Read, Show, Typeable, Data, Functor, Monoid, From !-})
#endif

data Field = F0
           | F1 { f1 :: Field  }
           | F2 { f2 :: Exp, f1 :: Field  }
           | F3 { f3 :: Exp, f2 :: Exp, f1 :: Field  }
#ifdef GHC_DERIVING
            deriving (Eq,Ord,Read,Show,Typeable,Data)
#else
            deriving ({-!  Eq, Ord, Enum, Bounded, Read, Show, Typeable, Data, Functor, Monoid, From !-})
#endif

newtype Id a = Id a 
#ifdef GHC_DERIVING
            deriving (Eq,Ord,Read,Show,Typeable,Data)
#else
              deriving ({-! Eq, Ord, Enum, Bounded, Read, Show, Typeable, Data, Functor, Monoid, From !-})
#endif

newtype Id2 a = Id2 { runId :: a }
#ifdef GHC_DERIVING
            deriving (Eq,Ord,Read,Show,Typeable,Data)
#else
            deriving ({-! Eq, Ord, Enum, Bounded, Read, Show, Typeable, Data, Functor, Monoid, From !-})
#endif