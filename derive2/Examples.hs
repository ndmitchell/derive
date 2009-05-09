
module Examples where

{-# DERIVE Eq #-}

-- only have () round == because they don't align properly
instance (Eq a) => Eq (Ctors a) where
    CtorZero == CtorZero = True
    CtorOne x1 x2 == CtorOne y1 y2 = (x1 == y1) && (x2 == y2) && True
    CtorTwo x1 == CtorTwo y1 = (x1 == y1) && True
    _ == _ = False

{-# DERIVE Arity #-}

instance Arity (Ctors a) where
    arity CtorZero{} = 0
    arity CtorOne{} = 2
    arity CtorTwo{} = 1

{-# DERIVE Enum #-}

instance Enum (Ctors a) where
    toEnum 0 = CtorZero{}
    toEnum 1 = CtorOne {}
    toEnum 2 = CtorTwo {}
    toEnum n = error $ "toEnum " ++ show n ++ ", not defined for " ++ "Ctors"

    fromEnum (CtorZero{}) = 0
    fromEnum (CtorOne {}) = 1
    fromEnum (CtorTwo {}) = 2
