{-|
    A pseudo derivation.  For each constructor in the data type,
    deriving @From@ generates @from@/CtorName/ which extracts the
    components if given the appropriate constructor, and crashes
    otherwise.  Unlike the DrIFT @\"From\"@ derivation, our version
    works for all constructors - zero-arity constructors always return
    @()@, arity-one constructors return the contained value, and all
    others return a tuple with all the components.
-}

module Data.Derive.From(makeFrom) where

{-

{-# TEST Sample #-}

fromFirst :: Sample a -> ()
fromFirst First = ()

fromSecond :: Sample a -> (a, a)
fromSecond (Second x1 x2) = (x1,x2)

fromThird :: Sample a -> a
fromThird (Third x1) = x1

-}

import Language.Haskell
import Data.Derive.Internal.Derivation


makeFrom :: Derivation
makeFrom = Derivation "From" $ \(_,d) -> Right $ concatMap (makeFromCtor d) $ dataDeclCtors d


makeFromCtor :: DataDecl -> CtorDecl -> [Decl]
makeFromCtor d c = [TypeSig sl [from] typ]
    where
        name = ctorDeclName c
        from = Ident $ "from" ++ name
        typ = TyFun
            (tyApp (TyCon $ UnQual $ Ident $ dataDeclName d) (map (TyVar . Ident) (dataDeclVars d)))
            (tyTuple $ map (fromBangType . snd) $ ctorDeclFields c)


tyTuple [x] = x
tyTuple xs = TyTuple Boxed xs



{-
from' "From"
from' dat = ((concatMap (\(ctorInd,ctor) ->
                         [SigD (mkName ("from" ++ ctorName ctor))
                               (ForallT (dataArgs dat) []
                                        (AppT (AppT ArrowT
                                                    (lK (dataName dat) (map VarT $ dataArgs dat)))
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
-}
