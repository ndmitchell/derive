
module Examples where

{-# DERIVE Eq #-}

-- only have () round == because they don't align properly
instance (Eq a) => Eq (Sample a) where
    First == First = True
    Second x1 x2 == Second y1 y2 = (x1 == y1) && (x2 == y2) && True
    Third x1 == Third y1 = (x1 == y1) && True
    _ == _ = False

{-# DERIVE Arity #-}

instance Arity (Sample a) where
    arity First{} = 0
    arity Second{} = 2
    arity Third{} = 1

{-# DERIVE Enum #-}

instance Enum (Sample a) where
    toEnum 0 = First{}
    toEnum 1 = Second {}
    toEnum 2 = Third {}
    toEnum n = error $ "toEnum " ++ show n ++ ", not defined for Sample"

    fromEnum (First{}) = 0
    fromEnum (Second {}) = 1
    fromEnum (Third {}) = 2

{-# DERIVE EnumCyclic #-}

instance Enum (Sample a) where
    toEnum 0 = First{}
    toEnum 1 = Second {}
    toEnum 2 = Third {}
    toEnum n = error $ "toEnum " ++ show n ++ ", not defined for Sample"

    fromEnum (First{}) = 0
    fromEnum (Second {}) = 1
    fromEnum (Third {}) = 2


    succ a = if b == length [First{},Second{},Third{}] then toEnum 0 else toEnum (b+1)
        where b = fromEnum a

    pred a = if b == 0 then toEnum (length [First{},Second{},Third{}]) else toEnum (b-1)
        where b = fromEnum a

{-# DERIVE Bounded #-}

instance Bounded a => Bounded (Sample a) where
    minBound = head [First, Second (const minBound 1) (const minBound 2), Third (const minBound 1)]
    maxBound = head [Third (const maxBound 1), Second (const maxBound 1) (const maxBound 2), First]

{-# DERIVE Serial #-}

instance Serial a => Serial (Sample a) where
    series = cons0 First \/ cons2 Second \/ cons1 Third

    coseries rs d = [ \t -> case t of
                                First -> t0
                                Second x1 x2 -> t1 x1 x2
                                Third x1 -> t2 x1
                    | t0 <- alts0 rs d `const` First{}
                    , t1 <- alts2 rs d `const` Second{}
                    , t2 <- alts1 rs d `const` Third{} ]

{-# DERIVE Ord #-}

instance Ord a => Ord (Sample a) where
    compare a b = check a b
        where
            check (First) (First) = EQ
            check (Second x1 x2) (Second y1 y2) = compare x1 y1 `next` compare x2 y2 `next` EQ
            check (Third x1) (Third y1) = compare x1 y1 `next` EQ
            check x y = compare (tag x) (tag y)

            next EQ x = x
            next x _ = x

            tag (First{}) = 0
            tag (Second{}) = 1
            tag (Third{}) = 2

{-# DERIVE NFData #-}

instance (NFData a) => NFData (Sample a) where
    rnf (First) = ()
    rnf (Second x1 x2) = rnf x1 `seq` rnf x2 `seq` ()
    rnf (Third x1) = rnf x1 `seq` ()

{-# DERIVE Is #-}

isFirst (First{}) = True; isFirst _ = False
isSecond  (Second {}) = True; isSecond  _ = False
isThird  (Third {}) = True; isThird  _ = False

{-# DERIVE Binary #-}

instance Binary a => Binary (Sample a) where
    put (First) = do putWord8 0
    put (Second x1 x2) = do putWord8 1 ; put x1 ; put x2
    put (Third x1) = do putWord8 2; put x1

    get = do
        i <- getWord8
        case i of
            0 -> do return (First)
            1 -> do x1 <- get ; x2 <- get ; return (Second x1 x2)
            2 -> do x1 <- get ; return (Third x1)
            _ -> error "Corrupted binary data for Sample"

{-# DERIVE BinaryDefer #-}

instance BinaryDefer a => BinaryDefer (Sample a) where
    bothDefer = defer [\ ~(First) -> unit First
                      ,\ ~(Second x1 x2) -> unit Second << x1 << x2
                      ,\ ~(Third x1) -> unit Third << x1
                      ]

{-# DERIVE BinaryOld #-}

instance Binary a => Binary (Sample a) where
    put_ bh x = 
        case x of
            First -> do
                when useTag $ putByte bh 0
            Second x1 x2 -> do
                when useTag $ putByte bh 1
                put_ bh x1
                put_ bh x2
            Third x1 -> do
                when useTag $ putByte bh 2
                put_ bh x1
        where
            useTag = length [First{}, Second{}, Third{}] > 1

    get bh = do
        h <- if useTag then getByte bh else return 0
        case h of
            0 -> do
                return (First)
            1 -> do
                x1 <- get bh
                x2 <- get bh
                return (Second x1 x2)
            2 -> do
                x1 <- get bh
                return (Third x1)
            _ -> fail "invalid binary data found"
        where
            useTag = length [First{}, Second{}, Third{}] > 1

{-# DERIVE Arbitrary #-}

instance Arbitrary a => Arbitrary (Sample a) where
    arbitrary = do
        x <- choose (0,length [First{},Second{},Third{}] - 1)
        case x of
            0 -> do return (First)
            1 -> do x1 <- arbitrary
                    x2 <- arbitrary
                    return (Second x1 x2)
            2 -> do x1 <- arbitrary
                    return (Third x1)

    coarbitrary (First) = variant 0
    coarbitrary (Second x1 x2) = variant 1 . coarbitrary x1 . coarbitrary x2
    coarbitrary (Third x1) = variant 2 . coarbitrary x1

{-# DERIVE Arbitrary2 #-}

instance Arbitrary a => Arbitrary (Sample a) where
    arbitrary = do
        x <- choose (0,length [First{},Second{},Third{}] - 1)
        case x of
            0 -> do return (First)
            1 -> do x1 <- arbitrary
                    x2 <- arbitrary
                    return (Second x1 x2)
            2 -> do x1 <- arbitrary
                    return (Third x1)

{-# DERIVE Data #-}

instance (Data a, Typeable a) => Data (Sample a) where
    gfoldl k r (First) = r First
    gfoldl k r (Second x1 x2) = r Second `k` x1 `k` x2
    gfoldl k r (Third x1) = r Third `k` x1
