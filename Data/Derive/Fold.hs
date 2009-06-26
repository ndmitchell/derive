-- Contributed by Tim Newsham <newsham -AT- lava -DOT- net>

{-|
    A pseudo derivation.  Derive a (non-recursive) fold function for 
    the type which takes one function per alternative constructor.  Each
    function takes the same arguments as the constructor and returns
    a value.  When applied to a value the fold function applies the
    function for the matching constructor to the constructor fields.
    This provides a first-class alternative to pattern matching to
    deconstruct the data type.
-}
module Data.Derive.Fold(makeFold) where
{-
-}


import Language.Haskell.TH.All
import Data.Char
import Data.List

{-
data Computer = Laptop {weight :: Int, memory :: Int}
              | Desktop {memory :: Int, processor :: Int}

==>

foldComputer f1 f2 (Laptop x1 x2) = f1 x1 x2
foldComputer f2 f2 (Desktop x1 x2) = f2 x1 x2

-}

makeFold :: Derivation
makeFold = derivation fold' "Fold"

fold' dat = [funN (upper dn) body]
    where
        upper (x:xs) = "fold" ++ toUpper x : xs
        dn = dataName dat
        fpats = map VarP fnames
        fvars = map VarE fnames
        ctors = dataCtors dat
        fnames = map (mkName . ('f':) . show) [1..length ctors]
        body = [sclause (fpats ++ [ctp ctor 'x']) (deconstr fv ctor) 
                    | (ctor,fv) <- zip ctors fvars]
        deconstr f ctor = foldl AppE f (ctv ctor 'x')
