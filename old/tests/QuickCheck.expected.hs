module QuickCheckInstances where
import QuickCheck
import Test.QuickCheck

instance Eq t1 => Eq (Expr t1)
    where (==) (ELambda x1 x2) (ELambda y1
                                        y2) = (&&) ((==) x1 y1) ((==) x2 y2)
          (==) (EAp x1 x2) (EAp y1 y2) = (&&) ((==) x1 y1) ((==) x2 y2)
          (==) (EVar x1) (EVar y1) = (==) x1 y1
          (==) _ _ = False

instance Ord t1 => Ord (Expr t1)
    where compare a b = check a b
                      where check (ELambda x1 x2) (ELambda y1 y2) = compare (x1, x2) (y1,
                                                                                      y2)
                            check (EAp x1 x2) (EAp y1 y2) = compare (x1, x2) (y1, y2)
                            check (EVar x1) (EVar y1) = compare x1 y1
                            check a b = compare (tag a) (tag b)
                            tag (ELambda {}) = 0
                            tag (EAp {}) = 1
                            tag (EVar {}) = 2

instance Show t1 => Show (Expr t1)
    where showsPrec p (ELambda x1
                               x2) = showParen (p > 10) (showString "ELambda" . (showChar ' ' . (showsPrec 11 x1 . (showChar ' ' . showsPrec 11 x2))))
          showsPrec p (EAp x1
                           x2) = showParen (p > 10) (showString "EAp" . (showChar ' ' . (showsPrec 11 x1 . (showChar ' ' . showsPrec 11 x2))))
          showsPrec p (EVar x1) = showParen (p > 10) (showString "EVar" . (showChar ' ' . showsPrec 11 x1))

instance Arbitrary a => Arbitrary (Expr a)
    where arbitrary = do x <- choose (0, 2)
                         case x of
                             0 -> do x1 <- arbitrary
                                     x2 <- arbitrary
                                     return (ELambda x1 x2)
                             1 -> do x1 <- arbitrary
                                     x2 <- arbitrary
                                     return (EAp x1 x2)
                             2 -> do x1 <- arbitrary
                                     return (EVar x1)
          coarbitrary (ELambda x1
                               x2) = (.) (variant 0) ((.) (coarbitrary x1) (coarbitrary x2))
          coarbitrary (EAp x1
                           x2) = (.) (variant 1) ((.) (coarbitrary x1) (coarbitrary x2))
          coarbitrary (EVar x1) = (.) (variant 2) (coarbitrary x1)

instance Eq Primary
    where (==) (Red) (Red) = True
          (==) (Green) (Green) = True
          (==) (Blue) (Blue) = True
          (==) _ _ = False

instance Ord Primary
    where compare a b = check a b
                      where check (Red) (Red) = compare () ()
                            check (Green) (Green) = compare () ()
                            check (Blue) (Blue) = compare () ()
                            check a b = compare (tag a) (tag b)
                            tag (Red {}) = 0
                            tag (Green {}) = 1
                            tag (Blue {}) = 2

instance Show Primary
    where showsPrec p (Red) = showParen (p > 10) (showString "Red")
          showsPrec p (Green) = showParen (p > 10) (showString "Green")
          showsPrec p (Blue) = showParen (p > 10) (showString "Blue")

instance Arbitrary Primary
    where arbitrary = do x <- choose (0, 2)
                         case x of
                             0 -> return Red
                             1 -> return Green
                             2 -> return Blue
          coarbitrary (Red) = variant 0
          coarbitrary (Green) = variant 1
          coarbitrary (Blue) = variant 2

instance Eq t1 => Eq (Id t1)
    where (==) (Id x1) (Id y1) = (==) x1 y1
          (==) _ _ = False

instance Ord t1 => Ord (Id t1)
    where compare a b = check a b
                      where check (Id x1) (Id y1) = compare x1 y1
                            check a b = compare (tag a) (tag b)
                            tag (Id {}) = 0

instance Show t1 => Show (Id t1)
    where showsPrec p (Id x1) = showParen (p > 10) (showString "Id" . (showChar ' ' . showsPrec 11 x1))

instance Arbitrary a => Arbitrary (Id a)
    where arbitrary = do x1 <- arbitrary
                         return (Id x1)
          coarbitrary (Id x1) = (.) (variant 0) (coarbitrary x1)
