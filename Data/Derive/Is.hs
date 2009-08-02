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
makeIs = Derivation "Is" $ \(_,d) -> Right $ concatMap (makeIsCtor d) $ dataDeclCtors d


makeIsCtor :: DataDecl -> CtorDecl -> [Decl]
makeIsCtor d c =
        [TypeSig sl [name nam] (dataDeclType d `TyFun` tyCon "Bool")
        ,FunBind $ match : [defMatch | length (dataDeclCtors d) > 1]]
    where
        nam = "is" ++ ctorDeclName c
        
        match = Match sl (name nam) [PParen $ PRec (qname $ ctorDeclName c) []] Nothing (UnGuardedRhs $ con "True") (BDecls [])
        defMatch = Match sl (name nam) [PWildCard] Nothing (UnGuardedRhs $ con "False") (BDecls [])
