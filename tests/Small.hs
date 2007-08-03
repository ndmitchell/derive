module Small where

import Data.Generics

data Expr name = ELambda name (Expr name)
               | EAp (Expr name) (Expr name)
               | EVar name
                 deriving ({-! Eq, Ord, Read, Show, Typeable, Data !-})

