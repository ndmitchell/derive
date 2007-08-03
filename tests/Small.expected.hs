module SmallInstancess where
import Small
import Data.Generics
import Data.Monoid

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
    where readsPrec p0 r = readParen (p0 > 10) (\r0 -> [(ELambda x1 x2,
                                                         r3) | ("ELambda", r1) <- lex r0,
                                                               (x1, r2) <- readsPrec 11 r1,
                                                               (x2,
                                                                r3) <- readsPrec 11 r2]) r ++ (readParen (p0 > 10) (\r0 -> [(EAp x1 x2,
                                                                                                                             r3) | ("EAp",
                                                                                                                                    r1) <- lex r0,
                                                                                                                                   (x1,
                                                                                                                                    r2) <- readsPrec 11 r1,
                                                                                                                                   (x2,
                                                                                                                                    r3) <- readsPrec 11 r2]) r ++ readParen (p0 > 10) (\r0 -> [(EVar x1,
                                                                                                                                                                                                r2) | ("EVar",
                                                                                                                                                                                                       r1) <- lex r0,
                                                                                                                                                                                                      (x1,
                                                                                                                                                                                                       r2) <- readsPrec 11 r1]) r)

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

fromELambda (ELambda x1 x2) = (x1, x2)
fromEAp (EAp x1 x2) = (x1, x2)
fromEVar (EVar x1) = x1

isELambda (ELambda {}) = True
isELambda _ = False
isEAp (EAp {}) = True
isEAp _ = False
isEVar (EVar {}) = True
isEVar _ = False

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

instance Enum Primary
    where toEnum 0 = Red{}
          toEnum 1 = Green{}
          toEnum 2 = Blue{}
          toEnum n = error ((++) "toEnum " ((++) (show n) ", not defined for Primary"))
          fromEnum (Red {}) = 0
          fromEnum (Green {}) = 1
          fromEnum (Blue {}) = 2

instance Bounded Primary
    where minBound = Red
          maxBound = Blue

instance Read Primary
    where readsPrec p0 r = readParen (p0 > 10) (\r0 -> [(Red,
                                                         r1) | ("Red",
                                                                r1) <- lex r0]) r ++ (readParen (p0 > 10) (\r0 -> [(Green,
                                                                                                                    r1) | ("Green",
                                                                                                                           r1) <- lex r0]) r ++ readParen (p0 > 10) (\r0 -> [(Blue,
                                                                                                                                                                              r1) | ("Blue",
                                                                                                                                                                                     r1) <- lex r0]) r)

instance Show Primary
    where showsPrec p (Red) = showParen (p > 10) (showString "Red")
          showsPrec p (Green) = showParen (p > 10) (showString "Green")
          showsPrec p (Blue) = showParen (p > 10) (showString "Blue")

typename_Primary = mkTyCon "Primary"
instance Typeable Primary
    where typeOf _ = mkTyConApp typename_Primary []

instance Data Primary
    where gfoldl k r (Red) = r Red
          gfoldl k r (Green) = r Green
          gfoldl k r (Blue) = r Blue

fromRed (Red) = ()
fromGreen (Green) = ()
fromBlue (Blue) = ()

isRed (Red {}) = True
isRed _ = False
isGreen (Green {}) = True
isGreen _ = False
isBlue (Blue {}) = True
isBlue _ = False

instance Eq t1 => Eq (BinTree t1)
    where (==) (Leaf) (Leaf) = True
          (==) (Branch x1 x2 x3) (Branch y1
                                         y2
                                         y3) = (&&) ((==) x1 y1) ((&&) ((==) x2 y2) ((==) x3 y3))
          (==) _ _ = False

instance Ord t1 => Ord (BinTree t1)
    where compare a b = check a b
                      where check (Leaf) (Leaf) = compare () ()
                            check (Branch x1 x2 x3) (Branch y1 y2 y3) = compare (x1,
                                                                                 x2,
                                                                                 x3) (y1, y2, y3)
                            check a b = compare (tag a) (tag b)
                            tag (Leaf {}) = 0
                            tag (Branch {}) = 1

instance Read t1 => Read (BinTree t1)
    where readsPrec p0 r = readParen (p0 > 10) (\r0 -> [(Leaf,
                                                         r1) | ("Leaf",
                                                                r1) <- lex r0]) r ++ readParen (p0 > 10) (\r0 -> [(Branch x1 x2 x3,
                                                                                                                   r4) | ("Branch",
                                                                                                                          r1) <- lex r0,
                                                                                                                         (x1,
                                                                                                                          r2) <- readsPrec 11 r1,
                                                                                                                         (x2,
                                                                                                                          r3) <- readsPrec 11 r2,
                                                                                                                         (x3,
                                                                                                                          r4) <- readsPrec 11 r3]) r

instance Show t1 => Show (BinTree t1)
    where showsPrec p (Leaf) = showParen (p > 10) (showString "Leaf")
          showsPrec p (Branch x1
                              x2
                              x3) = showParen (p > 10) (showString "Branch" . (showChar ' ' . (showsPrec 11 x1 . (showChar ' ' . (showsPrec 11 x2 . (showChar ' ' . showsPrec 11 x3))))))

typename_BinTree = mkTyCon "BinTree"
instance Typeable1 BinTree
    where typeOf1 _ = mkTyConApp typename_BinTree []
instance Typeable a => Typeable (BinTree a)
    where typeOf = typeOfDefault

instance (Data t1, Typeable t1) => Data (BinTree t1)
    where gfoldl k r (Leaf) = r Leaf
          gfoldl k r (Branch x1 x2 x3) = k (k (k (r Branch) x1) x2) x3

fromLeaf (Leaf) = ()
fromBranch (Branch x1 x2 x3) = (x1, x2, x3)

instance Eq t1 => Eq (Id t1)
    where (==) (Id x1) (Id y1) = (==) x1 y1
          (==) _ _ = False

instance Ord t1 => Ord (Id t1)
    where compare a b = check a b
                      where check (Id x1) (Id y1) = compare x1 y1
                            check a b = compare (tag a) (tag b)
                            tag (Id {}) = 0

instance Enum (Id t1)
    where toEnum 0 = Id{}
          toEnum n = error ((++) "toEnum " ((++) (show n) ", not defined for Id"))
          fromEnum (Id {}) = 0

instance Bounded t1 => Bounded (Id t1)
    where minBound = Id minBound
          maxBound = Id maxBound

instance Read t1 => Read (Id t1)
    where readsPrec p0 r = readParen (p0 > 10) (\r0 -> [(Id x1,
                                                         r2) | ("Id", r1) <- lex r0,
                                                               (x1, r2) <- readsPrec 11 r1]) r

instance Show t1 => Show (Id t1)
    where showsPrec p (Id x1) = showParen (p > 10) (showString "Id" . (showChar ' ' . showsPrec 11 x1))

typename_Id = mkTyCon "Id"
instance Typeable1 Id
    where typeOf1 _ = mkTyConApp typename_Id []
instance Typeable a => Typeable (Id a)
    where typeOf = typeOfDefault

instance (Data t1, Typeable t1) => Data (Id t1)
    where gfoldl k r (Id x1) = k (r Id) x1

instance Functor Id
    where fmap fun (Id a) = Id (fun a)

instance Monoid t1 => Monoid (Id t1)
    where mempty = Id mempty
          mappend (Id x1) (Id y1) = Id (mappend x1 y1)

fromId (Id x1) = x1

instance Eq t1 => Eq (Id2 t1)
    where (==) (Id2 x1) (Id2 y1) = (==) x1 y1
          (==) _ _ = False

instance Ord t1 => Ord (Id2 t1)
    where compare a b = check a b
                      where check (Id2 x1) (Id2 y1) = compare x1 y1
                            check a b = compare (tag a) (tag b)
                            tag (Id2 {}) = 0

instance Enum (Id2 t1)
    where toEnum 0 = Id2{}
          toEnum n = error ((++) "toEnum " ((++) (show n) ", not defined for Id2"))
          fromEnum (Id2 {}) = 0

instance Bounded t1 => Bounded (Id2 t1)
    where minBound = Id2 minBound
          maxBound = Id2 maxBound

instance Read t1 => Read (Id2 t1)
    where readsPrec p0 r = readParen False (\r0 -> [(Id2 x1,
                                                     r6) | ("Id2", r1) <- lex r0,
                                                           ("{", r2) <- lex r1,
                                                           ("runId", r3) <- lex r2,
                                                           ("=", r4) <- lex r3,
                                                           (x1, r5) <- readsPrec 0 r4,
                                                           ("}", r6) <- lex r5]) r

instance Show t1 => Show (Id2 t1)
    where showsPrec p (Id2 x1) = showString "Id2 {" . (showChar ' ' . (showString "runId = " . (showsPrec 0 x1 . (showChar ' ' . showChar '}'))))

typename_Id2 = mkTyCon "Id2"
instance Typeable1 Id2
    where typeOf1 _ = mkTyConApp typename_Id2 []
instance Typeable a => Typeable (Id2 a)
    where typeOf = typeOfDefault

instance (Data t1, Typeable t1) => Data (Id2 t1)
    where gfoldl k r (Id2 x1) = k (r Id2) x1

instance Functor Id2
    where fmap fun (Id2 a) = Id2 (fun a)

instance Monoid t1 => Monoid (Id2 t1)
    where mempty = Id2 mempty
          mappend (Id2 x1) (Id2 y1) = Id2 (mappend x1 y1)

setRunId v x = x{runId = v}

setRunId a0 b0 = Id2 a0

fromId2 (Id2 x1) = x1
