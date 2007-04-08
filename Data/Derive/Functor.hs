
module Data.Derive.Functor(makeFunctor) where

import Language.Haskell.TH.All
import Data.List


makeFunctor :: Derivation
makeFunctor = Derivation derive "Functor"

derive dat
 | dataArity dat == 0 = []
 | otherwise          = generic_instance (classFor arg) dat [] [funN (fmapFor arg) body]
    where
        arg  = Arg False 1 -- TODO : make a parameter?
        body = map (deriveFunctorCtor dat arg) (dataCtors dat)

-- | Derive Functor over a given argument number for a type
--   return (derived function, required instances)
deriveFunctorCtor :: DataDef -> Arg -> CtorDef -> Clause
deriveFunctorCtor dat arg ctor = sclause lhs rhs
    where
       name = ctorName ctor
       types = ctorRTypes dat ctor
       arity = length types
       args = map return $ take arity ['a'..]
       lhs = [vr "fun", lK name (map vr args)]
       rhs = lK name $ zipWith app (map (deriveFunctorType arg) types) (map vr args)

-- | Derive Functor over a given argument number for a type
--   return (derived function, required instances)
deriveFunctorType :: Arg -> RType -> Exp
deriveFunctorType arg (RType (TypeCon "->") [a,b]) -- a -> b
  | isId af && isId bf  =  id'
  | isId af             =  InfixE Nothing   (l0 ".") (Just bf)
  | isId bf             =  InfixE (Just af) (l0 ".") Nothing
  | otherwise           =  LamE [l0 "arg"] $ l2 "." af (l2 "." (l0 "arg") bf)
    where af = deriveFunctorType arg{co=not (co arg)} a
          bf = deriveFunctorType arg                  b
deriveFunctorType arg (RType tycon args)
    = foldl  composeAp  (deriveFunctorCon arg tycon)
                        (zipWith fmapAp (map (Arg False) [0..])
                                        (map (deriveFunctorType arg) args))

-- | Derive Functor over a given argument number for a type constructor
deriveFunctorCon :: Arg -> TypeCon -> Exp
deriveFunctorCon (Arg False i) (TypeArg j) | i == j = l0 "fun"
deriveFunctorCon (Arg True  i) (TypeArg j) | i == j = error "argument used in contravariant position"
deriveFunctorCon _             _                    = id'


-- | The identity function
id' :: Exp
id' = l0 "id"

-- | Is a function the identity function?
isId = (== id')

-- | optimized (.) application
composeAp a b
 | isId a    = b
 | isId b    = a
 | otherwise = l2 "." a b

-- | optimized fmap application
fmapAp arg b
 | isId b    = id'
 | otherwise = l1 (fmapFor arg) b

-- | optimized application
app a b
 | isId a    = b
 | otherwise = a `AppE` b

-- | Derive Functor or CoFunctor over an argument
data Arg = Arg { co :: Bool, position :: Int }

fmapFor  (Arg co i) = (if co then "co" else "") ++ "fmap"    ++ (if i > 1 then show i else "")
classFor (Arg co i) = (if co then "Co" else "") ++ "Functor" ++ (if i > 1 then show i else "")
