-- | A Pseudo derivation. For every label, creates a function
-- foo_u and foo_s which updates and sets the label respectively,
-- e.g. 'foo_u (+1) bar' or 'foo_s 10 baz'
module Data.Derive.Update(makeUpdate) where

import Language.Haskell.TH.All
import Control.Monad (liftM2)
import Data.Char
import Data.List
{-
data Computer = Laptop {weight :: Int, memory :: Int}
              | Desktop {memory :: Int, processor :: Int}

==>

weight_u f r = r{weight = f (weight r)}
weight_s v = weight_u (const v)
memory_u f r = r{memory = f (memory r)}
memory_s v = memory_u (const v)
processor_u f r = r{processor = f (processor r)}
processor_s v = processor_u (const v)

-}

makeUpdate :: Derivation
makeUpdate = derivation update' "Update"

update' dat = concatMap f fields
    where
        ctors = dataCtors dat -- constructors of the data type

        -- get all of the fields of every data constructor
        fields = nub $ concatMap (liftM2 zip ctorFields ctorTypes) ctors

        tyargs = dataArgs dat
        rty = lK (dataName dat) (map VarT tyargs)
        funT a b = AppT (AppT ArrowT a) b

        f (fname, fty) =
            [ sigN (fname ++ "_u") $ ForallT tyargs [] (funT (funT fty fty) (funT rty rty))
            , funN (fname ++ "_u") $
               [sclause [vr "f",vr "r"]
                            $ RecUpdE (vr "r") [( mkName fname
                                                , AppE (vr "f") $ AppE (vr fname) (vr "r"))]]
            , sigN (fname ++ "_s") $ ForallT tyargs [] (funT fty (funT rty rty))
            , funN (fname ++ "_s") $
                 [sclause [vr "v"] $ AppE (VarE (mkName $ fname++"_u"))
                                          (AppE (vr "const") $ vr "v")]
            ]
