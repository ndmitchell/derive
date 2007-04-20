-- NOTE: Cannot be guessed as it inducts on the data type (not its constructors)

-- | Type level typeable, as used by the undecidable-instances version
-- of the @TypeEq@ class from Oleg and Lammel's HList library.  The
-- type numbers used are autmatically generated using a little-endian
-- @Enum@ representation of the type's name.  Note that we truncate
-- characters above 255, and ignore the module name, though the
-- consequences are much less severe than in the @Typeable@ case since
-- no coercion is involved.
module Data.Derive.TTypeable(makeTTypeable) where

import Language.Haskell.TH.All
import Data.Char
import Data.Bits

makeTTypeable :: Derivation
makeTTypeable = Derivation ttypeable' "TTypeable"
ttypeable' dat = [InstanceD ctx hd []]
    where
        bits x = [ if x .&. mask /= 0 then hSucc' hZero' else hZero' | mask <- [1,2,4,8,16,32,64,128] ]
        bitnam = foldr hCons' hNil' (concatMap (bits . ord) (dataName dat))

        hd = l2 "TTypeable" (lK (dataName dat) (vrs 't')) (foldr hCons' hNil' (bitnam : vrs 'n'))
        ctx = zipWith (l2 "TTypeable") (vrs 't') (vrs 'n')

        vrs = flip vars (dataArity dat)
