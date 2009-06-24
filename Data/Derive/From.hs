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
makeFromCtor d c = [TypeSig sl [name from] typ, FunBind [match]]
    where
        n = ctorDeclName c
        from = "from" ++ n

        typ = TyFun
            (tyApp (tyCon $ dataDeclName d) (map tyVar $ dataDeclVars d))
            (tyTuple $ map (fromBangType . snd) $ ctorDeclFields c)

        match = Match sl (name from) [pat] Nothing (UnGuardedRhs rhs) (BDecls [])
        pat = (length vars == 0 ? id $ PParen) $ PApp (qname n) (map pVar vars)
        vars = take (length $ ctorDeclFields c) $ map ((:) 'x' . show) [1..]
        rhs = valTuple $ map var vars


tyTuple [] = TyCon $ Special UnitCon
tyTuple [x] = x
tyTuple xs = TyTuple Boxed xs


valTuple [] = Con $ Special UnitCon
valTuple [x] = x
valTuple xs = Tuple xs
