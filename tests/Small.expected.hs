module SmallInstancess where
import Small
import Data.Generics

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

instance Read t1 => Read (Expr t1)
    where readsPrec p0 r0 = readParen (p0 > 10) [(ELambda x1 x2,
                                                  r3) | ("ELambda", r1) <- lex r0,
                                                        (x1, r2) <- readsPrec 11 r1,
                                                        (x2,
                                                         r3) <- readsPrec 11 r2] ++ (readParen (p0 > 10) [(EAp x1 x2,
                                                                                                           r3) | ("EAp",
                                                                                                                  r1) <- lex r0,
                                                                                                                 (x1,
                                                                                                                  r2) <- readsPrec 11 r1,
                                                                                                                 (x2,
                                                                                                                  r3) <- readsPrec 11 r2] ++ readParen (p0 > 10) [(EVar x1,
                                                                                                                                                                   r2) | ("EVar",
                                                                                                                                                                          r1) <- lex r0,
                                                                                                                                                                         (x1,
                                                                                                                                                                          r2) <- readsPrec 11 r1])

instance Show t1 => Show (Expr t1)
    where showsPrec p (ELambda x1
                               x2) = showParen (p > 10) (showString "ELambda" . (showChar ' ' . (showsPrec 11 x1 . (showChar ' ' . showsPrec 11 x2))))
          showsPrec p (EAp x1
                           x2) = showParen (p > 10) (showString "EAp" . (showChar ' ' . (showsPrec 11 x1 . (showChar ' ' . showsPrec 11 x2))))
          showsPrec p (EVar x1) = showParen (p > 10) (showString "EVar" . (showChar ' ' . showsPrec 11 x1))

typename_Expr = mkTyCon "Expr"
instance Typeable1 Expr
    where typeOf1 _ = mkTyConApp typename_Expr []
instance Typeable a => Typeable (Expr a)
    where typeOf = typeOfDefault

instance (Data t1, Typeable t1) => Data (Expr t1)
    where gfoldl k r (ELambda x1 x2) = k (k (r ELambda) x1) x2
          gfoldl k r (EAp x1 x2) = k (k (r EAp) x1) x2
          gfoldl k r (EVar x1) = k (r EVar) x1
