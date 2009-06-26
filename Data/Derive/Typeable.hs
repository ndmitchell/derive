{-|
    Derivation for the 'Typeable' class, as described in the Scrap
    Your Boilerplate papers.  This derivation generates instances for
    all kinds of TypeableK classes; as such we do NOT require the
    GHC-specific generic downkinding instances to provide lower kind
    instances.

    Also creates a @typename_\<the type name\>@ value to hold the
    'TypeRep'.
-}
module Data.Derive.Typeable(makeTypeable) where
{-

{-# TEST Bool #-}
typename_Bool :: TyCon
typename_Bool = mkTyCon "Example.Bool"
instance Typeable Bool where
    typeOf _ = mkTyConApp typename_Bool []

{-# TEST Sample #-}

typename_Sample :: TyCon
typename_Sample = mkTyCon "Example.Sample"
instance Typeable1 Sample where
    typeOf1 _ = mkTyConApp typename_Sample []
instance Typeable a => Typeable (Sample a) where
    typeOf = typeOfDefault

{-# TEST Either #-}

typename_Either :: TyCon
typename_Either = mkTyCon "Example.Either"
instance Typeable2 Either where
    typeOf2 _ = mkTyConApp typename_Either []
instance Typeable a => Typeable1 (Either a) where
    typeOf1 = typeOf1Default
instance (Typeable a, Typeable b) => Typeable (Either a b) where
    typeOf = typeOfDefault
-}

import Language.Haskell
import Data.Derive.Internal.Derivation
import Data.List

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
makeTypeable = Derivation "Typeable" $ \(ModuleName modu,x) -> Right $ mkTypeable modu x


mkTypeable :: String -> DataDecl -> [Decl]
mkTypeable modu d =
    [TypeSig sl [name fun] (tyCon "TyCon")] ++
    [PatBind sl (pVar fun) Nothing (UnGuardedRhs bod) (BDecls []) |
        let bod = App (var "mkTyCon") (Lit $ String $ modu ++ "." ++ nam)] ++
    [inst [] (showN n) [tyCon nam] [PWildCard] $ app (var "mkTyConApp") [var fun, List []]] ++
    [inst [ClassA (qname "Typeable") [v] | v <- tvs] i [tyApp (tyCon nam) tvs] [] $ var $ "typeOf" ++ i ++ "Default"
        | (vs,i) <- zip (tail $ inits $ dataDeclVars d) $ map showN [n-1,n-2..]
        , let tvs = map tyVar vs]
    where
        nam = dataDeclName d
        fun = "typename_" ++ nam
        n = length $ dataDeclVars d


inst ctxt n typ pat expr =
    InstDecl sl ctxt (qname $ "Typeable" ++ n) typ
        [InsDecl $ bind ("typeOf" ++ n) pat expr]

showN 0 = ""
showN n = show n
