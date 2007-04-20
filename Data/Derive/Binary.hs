-- NOTE: Cannot be guessed as is not inductive because of getWordN

-- | Derivation for Data.Binary's Binary class for serializing values.
-- The generated instances implement a very simple tagged data format.
-- First, the (0-based) constructor number is stored, in the smallest
-- of 0, 1, 2, or 4 bytes that can represent the entire range.  Then,
-- the constructor's arguments are stored, in order, using the Binary
-- instances in scope.
module Data.Derive.Binary(makeBinary) where

import Language.Haskell.TH.All
import Data.List

makeBinary :: Derivation
makeBinary = Derivation derive "Binary"

derive dat =
        simple_instance "Binary" dat [funN "put" pbody, funN "get" gbody]
    where
        pbody = [ sclause [ctp ctor 'x'] (put_case nm ctor) | (nm,ctor) <- items ]
        put_case nm ctor = sequence__ (ptag (lit nm) : map (l1 "put") (ctv ctor 'x'))

        gbody = [sclause [] (gtag >>=: ("tag_" ->: case' (vr "tag_") (map get_case items)))]
        get_case (nm,ctor) = (lit nm, liftmk (ctc ctor) (replicate (ctorArity ctor) (vr "get")))

        ctors = dataCtors dat
        nctors = length ctors
        items :: [(Integer,CtorDef)]
        items = zip [0..] ctors

        (ptag, gtag) | nctors <= 1     = (\_ -> return' unit, return' (lit (0::Integer)))
                     | nctors <= 256   = (l1 "putWord8", l0 "getWord8")
                     | nctors <= 65536 = (l1 "putWord16", l0 "getWord16")
                     | otherwise       = (l1 "putWord32", l0 "getWord32")
