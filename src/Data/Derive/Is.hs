module Data.Derive.Is(makeIs) where
{-

test :: Sample

isFirst :: Sample a -> Bool
isFirst (First{}) = True ; isFirst _ = False

isSecond :: Sample a -> Bool
isSecond (Second{}) = True ; isSecond _ = False

isThird :: Sample a -> Bool
isThird (Third{}) = True ; isThird _ = False

-}

import Language.Haskell
import Data.Derive.Internal.Derivation


makeIs :: Derivation
makeIs = derivationCustom "Is" $ \(_,d) -> Right $ concatMap (makeIsCtor d) $ dataDeclCtors d


makeIsCtor :: DataDecl -> CtorDecl -> [Decl ()]
makeIsCtor d c = if not $ isIdent $ ctorDeclName c then [] else
        [TypeSig () [name nam] (TyFun () (dataDeclType d) (tyCon "Bool"))
        ,FunBind () $ match : [defMatch | length (dataDeclCtors d) > 1]]
    where
        nam = "is" ++ ctorDeclName c
        
        match = Match () (name nam) [PParen () $ PRec () (qname $ ctorDeclName c) []] (UnGuardedRhs () $ con "True") Nothing
        defMatch = Match () (name nam) [PWildCard ()] (UnGuardedRhs () $ con "False") Nothing
