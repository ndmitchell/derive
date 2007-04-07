
module Data.Derive.Binary(makeBinary) where

import Data.Derive
import Language.Haskell.TH.Peephole
import Data.List

makeBinary :: Derivation
makeBinary = Derivation derive "Binary"

derive dat@(DataDef name arity ctors) = peephole $
        simple_instance "Binary" dat [funN "put" pbody, funN "get" gbody]
    where
        pbody = [ sclause [ctp ctor 'x'] (put_case nm ctor) | (nm,ctor) <- items ]
        put_case nm ctor = sequ' (ptag (lit nm) : map (l1 "put") (ctv ctor 'x'))

        gbody = [sclause [] (gtag >>=: ("tag_" ->: case' (vr "tag_") (map get_case items)))]
        get_case (nm,ctor) = (lit nm, liftmk (ctc ctor) (replicate (ctorArity ctor) (vr "get")))

        nctors = length ctors
        items :: [(Integer,CtorDef)]
        items = zip [0..] ctors

        (ptag, gtag) | nctors <= 1     = (\_ -> l1 "return" (lit ()), l1 "return" (lit (0::Integer)))
                     | nctors <= 256   = (l1 "putWord8", l0 "getWord8")
                     | nctors <= 65536 = (l1 "putWord16", l0 "getWord16")
                     | otherwise       = (l1 "putWord32", l0 "getWord32")
