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

test :: Sample

fromFirst :: Sample a -> ()
fromFirst First = ()
fromFirst _ = error "fromFirst failed, not a First"

fromSecond :: Sample a -> (a, a)
fromSecond (Second x1 x2) = (x1,x2)
fromSecond _ = error "fromSecond failed, not a Second"

fromThird :: Sample a -> a
fromThird (Third x1) = x1
fromThird _ = error "fromThird failed, not a Third"

-}

import Language.Haskell
import Data.Derive.Internal.Derivation


makeFrom :: Derivation
makeFrom = derivationCustom "From" $ \(_,d) -> Right $ concatMap (makeFromCtor d) $ dataDeclCtors d


makeFromCtor :: DataDecl -> CtorDecl -> [Decl ()]
makeFromCtor d c | isIdent n = [TypeSig () [name from] typ, FunBind () $ match : [defMatch | length (dataDeclCtors d) > 1]]
                 | otherwise = []
    where
        n = ctorDeclName c
        from = "from" ++ n

        typ = TyFun () (dataDeclType d)
            (tyTuple $ map snd $ ctorDeclFields c)

        match = Match () (name from) [pat] (UnGuardedRhs () rhs) Nothing
        pat = (length vars == 0 ? id $ PParen ()) $ PApp () (qname n) (map pVar vars)
        vars = take (length $ ctorDeclFields c) $ map ((:) 'x' . show) [1..]
        rhs = valTuple $ map var vars

        defMatch = Match () (name from) [PWildCard ()] (UnGuardedRhs () err) Nothing
        err = App () (var "error") $ Lit () $ let s = from ++ " failed, not a " ++ n in String () s (show s)


tyTuple [] = TyCon () $ Special () $ UnitCon ()
tyTuple [x] = x
tyTuple xs = TyTuple () Boxed xs


valTuple [] = Con () $ Special () $ UnitCon ()
valTuple [x] = x
valTuple xs = Tuple () Boxed xs
