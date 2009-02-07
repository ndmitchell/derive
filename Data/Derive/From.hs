{-# OPTIONS_GHC -fth -cpp #-}

-- | A pseudo derivation.  For each constructor in the data type,
-- deriving @From@ generates @from@/CtorName/ which extracts the
-- components if given the appropriate constructor, and crashes
-- otherwise.  Unlike the DrIFT @\"From\"@ derivation, our version
-- works for all constructors - zero-arity constructors always return
-- @()@, arity-one constructors return the contained value, and all
-- others return a tuple with all the components.
module Data.Derive.From(makeFrom) where

import Language.Haskell.TH.All

-- don't use this until Guess supports type signatures!
#ifdef GUESS

import Data.DeriveGuess

example = (,) "From" [d|

    fromCtorZero (CtorZero) = ()
    fromCtorOne  (CtorOne x1) = tup1 x1
    fromCtorTwo  (CtorTwo x1 x2) = (x1,x2)
    fromCtorTwo' (CtorTwo' x1 x2) = (x1,x2)

    |]

#endif

makeFrom :: Derivation
makeFrom = derivation from' "From"
from' dat = ((concatMap (\(ctorInd,ctor) ->
                         [SigD (mkName ("from" ++ ctorName ctor))
                               (ForallT (ex_args dat) []
                                        (AppT (AppT ArrowT
                                                    (lK (dataName dat) (map VarT $ ex_args dat)))
                                              (tup (ctorTypes ctor))))
                         ,FunD (mkName ("from" ++ ctorName ctor))
                               [(Clause [(ConP (mkName ("" ++ ctorName ctor))
                                               ((map (\field -> (VarP (mkName ("x" ++ show field))))
                                                     (id [1..ctorArity ctor]))++[]))]
                                        (NormalB (TupE ((map (\field -> (VarE (mkName ("x" ++ show field))))
                                                             (id [1..ctorArity ctor]))++[])))
                                                 [])]
                         ])
             (id (zip [0..] (dataCtors dat))))++[])
