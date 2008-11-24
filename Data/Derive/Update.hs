-- | A Pseudo derivation. For every label, creates a function
-- foo_u and foo_s which updates and sets the label respectively,
-- e.g. 'foo_u (+1) bar' or 'foo_s 10 baz'
module Data.Derive.Update(makeUpdate) where

import Language.Haskell.TH.All
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
        fields = nub $ concatMap ctorFields ctors -- get all of the fields of every data type
        
        f field = [ funN (field ++ "_u") $
                     [sclause [vr "f",vr "r"] 
                                  $ RecUpdE (vr "r") [( mkName field
                                                      , AppE (vr "f") $ AppE (vr field) (vr "r"))]]
                  , funN (field ++ "_s") $
                       [sclause [vr "v"] $ AppE (VarE (mkName $ field++"_u")) 
                                                (AppE (vr "const") $ vr "v")]
                  ]