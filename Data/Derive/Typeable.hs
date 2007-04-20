-- NOTE: Cannot be guessed as it inducts on the data type (not its constructors)

-- | Derivation for the 'Typeable' class, as described in the Scrap
-- Your Boilerplate papers.  This derivation generates instances for
-- all kinds of TypeableK classes; as such we do NOT require the
-- GHC-specific generic downkinding instances to provide lower kind
-- instances.
--
-- The generated 'TypeRep' uses only the base name of the type, so
-- identically named types in different modules can be treated as the
-- same, with disasterous consequences.
--
-- Also creates a @typename_\<the type name\>@ value to hold the
-- 'TypeRep'.

module Data.Derive.Typeable(makeTypeable) where

import Language.Haskell.TH.All
import Data.Char

-- based on the macros in: http://darcs.haskell.org/packages/base/include/Typeable.h

{-
#define INSTANCE_TYPEABLE1(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable1 tycon where { typeOf1 _ = mkTyConApp tcname [] }; \
instance Typeable a => Typeable (tycon a) where { typeOf = typeOfDefault }

#define INSTANCE_TYPEABLE2(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable2 tycon where { typeOf2 _ = mkTyConApp tcname [] }; \
instance Typeable a => Typeable1 (tycon a) where { \
  typeOf1 = typeOf1Default }; \
instance (Typeable a, Typeable b) => Typeable (tycon a b) where { \
  typeOf = typeOfDefault }

#define INSTANCE_TYPEABLE3(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable3 tycon where { typeOf3 _ = mkTyConApp tcname [] }; \
instance Typeable a => Typeable2 (tycon a) where { \
  typeOf2 = typeOf2Default }; \
instance (Typeable a, Typeable b) => Typeable1 (tycon a b) where { \
  typeOf1 = typeOf1Default }; \
instance (Typeable a, Typeable b, Typeable c) => Typeable (tycon a b c) where { \
  typeOf = typeOfDefault }
-}


makeTypeable :: Derivation
makeTypeable = Derivation typeable' "Typeable"
typeable' dat = (funN nam [sclause [] (l1 "mkTyCon" $ lit $ dataName dat)])
                : map f [0..dataArity dat]
    where
        nam = [if x == '.' then '_' else x | x <- "typename_" ++ dataName dat]

        f n = InstanceD (map (l1 "Typeable") vars)
                        (l1 ("Typeable"++sn) $ lK (dataName dat) vars)
                        [funN ("typeOf"++sn) [sclause [WildP | n == 0] def]]
            where
                def = if n == 0 then l2 "mkTyConApp" (l0 nam) (lst [])
                                else l0 ("typeOf" ++ sn ++ "Default")
                vars = take n $ map (vr . return) ['a'..]
                sn = let i = dataArity dat - n in if i == 0 then "" else show i
