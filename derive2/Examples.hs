
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
    toEnum n = error $ "toEnum " ++ show n ++ ", not defined for Ctors"

    fromEnum (CtorZero{}) = 0
    fromEnum (CtorOne {}) = 1
    fromEnum (CtorTwo {}) = 2

{-# DERIVE EnumCyclic #-}

instance Enum (Ctors a) where
    toEnum 0 = CtorZero{}
    toEnum 1 = CtorOne {}
    toEnum 2 = CtorTwo {}
    toEnum n = error $ "toEnum " ++ show n ++ ", not defined for Ctors"

    fromEnum (CtorZero{}) = 0
    fromEnum (CtorOne {}) = 1
    fromEnum (CtorTwo {}) = 2


    succ a = if b == length [CtorZero{},CtorOne{},CtorTwo{}] then toEnum 0 else toEnum (b+1)
        where b = fromEnum a

    pred a = if b == 0 then toEnum (length [CtorZero{},CtorOne{},CtorTwo{}]) else toEnum (b-1)
        where b = fromEnum a

{-# DERIVE Bounded #-}

instance Bounded a => Bounded (Ctors a) where
    minBound = head [CtorZero, CtorOne (const minBound 1) (const minBound 2), CtorTwo (const minBound 1)]
    maxBound = head [CtorTwo (const maxBound 1), CtorOne (const maxBound 1) (const maxBound 2), CtorZero]

{-# DERIVE Serial #-}

instance Serial a => Serial (Ctors a) where
    series = cons0 CtorZero \/ cons2 CtorOne \/ cons1 CtorTwo

    coseries rs d = [ \t -> case t of
                                CtorZero -> t0
                                CtorOne x1 x2 -> t1 x1 x2
                                CtorTwo x1 -> t2 x1
                    | t0 <- alts0 rs d `const` CtorZero{}
                    , t1 <- alts2 rs d `const` CtorOne{}
                    , t2 <- alts1 rs d `const` CtorTwo{} ]

{-# DERIVE Ord #-}

instance Ord a => Ord (Ctors a) where
    compare a b = check a b
        where
            check (CtorZero) (CtorZero) = EQ
            check (CtorOne x1 x2) (CtorOne y1 y2) = compare x1 y1 `next` compare x2 y2 `next` EQ
            check (CtorTwo x1) (CtorTwo y1) = compare x1 y1 `next` EQ
            check x y = compare (tag x) (tag y)

            next EQ x = x
            next x _ = x

            tag (CtorZero{}) = 0
            tag (CtorOne{}) = 1
            tag (CtorTwo{}) = 2

{-# DERIVE NFData #-}

instance (NFData a) => NFData (Ctors a) where
    rnf (CtorZero) = ()
    rnf (CtorOne x1 x2) = rnf x1 `seq` rnf x2 `seq` ()
    rnf (CtorTwo x1) = rnf x1 `seq` ()

{-# DERIVE Is #-}

isCtorZero (CtorZero{}) = True; isCtorZero _ = False
isCtorOne  (CtorOne {}) = True; isCtorOne  _ = False
isCtorTwo  (CtorTwo {}) = True; isCtorTwo  _ = False

{-# DERIVE Binary #-}

instance Binary a => Binary (Ctors a) where
    put (CtorZero) = do putWord8 0
    put (CtorOne x1 x2) = do putWord8 1 ; put x1 ; put x2
    put (CtorTwo x1) = do putWord8 2; put x1

    get = do
        i <- getWord8
        case i of
            0 -> do return (CtorZero)
            1 -> do x1 <- get ; x2 <- get ; return (CtorOne x1 x2)
            2 -> do x1 <- get ; return (CtorTwo x1)
            _ -> error "Corrupted binary data for Ctors"

{-# DERIVE BinaryDefer #-}

instance BinaryDefer a => BinaryDefer (Ctors a) where
    bothDefer = defer [\ ~(CtorZero) -> unit CtorZero
                      ,\ ~(CtorOne x1 x2) -> unit CtorOne << x1 << x2
                      ,\ ~(CtorTwo x1) -> unit CtorTwo << x1
                      ]

{-# DERIVE BinaryOld #-}

instance Binary a => Binary (Ctors a) where
    put_ bh x = 
        case x of
            CtorZero -> do
                when useTag $ putByte bh 0
            CtorOne x1 x2 -> do
                when useTag $ putByte bh 1
                put_ bh x1
                put_ bh x2
            CtorTwo x1 -> do
                when useTag $ putByte bh 2
                put_ bh x1
        where
            useTag = length [CtorZero{}, CtorOne{}, CtorTwo{}] > 1

    get bh = do
        h <- if useTag then getByte bh else return 0
        case h of
            0 -> do
                return (CtorZero)
            1 -> do
                x1 <- get bh
                x2 <- get bh
                return (CtorOne x1 x2)
            2 -> do
                x1 <- get bh
                return (CtorTwo x1)
            _ -> fail "invalid binary data found"
        where
            useTag = length [CtorZero{}, CtorOne{}, CtorTwo{}] > 1

{-# DERIVE Arbitrary #-}

instance Arbitrary a => Arbitrary (Ctors a) where
    arbitrary = do
        x <- choose (0,length [CtorZero{},CtorOne{},CtorTwo{}] - 1)
        case x of
            0 -> do return (CtorZero)
            1 -> do x1 <- arbitrary
                    x2 <- arbitrary
                    return (CtorOne x1 x2)
            2 -> do x1 <- arbitrary
                    return (CtorTwo x1)

    coarbitrary (CtorZero) = variant 0
    coarbitrary (CtorOne x1 x2) = variant 1 . coarbitrary x1 . coarbitrary x2
    coarbitrary (CtorTwo x1) = variant 2 . coarbitrary x1

{-# DERIVE Arbitrary2 #-}

instance Arbitrary a => Arbitrary (Ctors a) where
    arbitrary = do
        x <- choose (0,length [CtorZero{},CtorOne{},CtorTwo{}] - 1)
        case x of
            0 -> do return (CtorZero)
            1 -> do x1 <- arbitrary
                    x2 <- arbitrary
                    return (CtorOne x1 x2)
            2 -> do x1 <- arbitrary
                    return (CtorTwo x1)

{-# DERIVE Data #-}

instance (Data a, Typeable a) => Data (Ctors a) where
    gfoldl k r (CtorZero) = r CtorZero
    gfoldl k r (CtorOne x1 x2) = r CtorOne `k` x1 `k` x2
    gfoldl k r (CtorTwo x1) = r CtorTwo `k` x1
