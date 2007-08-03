{-
    This module is not written/maintained by the usual Data.Derive author.

    MAINTAINER: Twan van Laarhoven 
    EMAIL: "twanvl" ++ "@" ++ "gmail" ++ "." ++ "com"

    Please send all patches to this module to Neil (ndmitchell -at- gmail),
    and CC Twan.
-}

-- NOTE: Cannot be guessed as it relies on type information

-- | Derives 'Functor', as discussed on the Haskell-prime mailing list:
-- <http://www.mail-archive.com/haskell-prime@haskell.org/msg02116.html>.
module Data.Derive.Functor(makeFunctor) where

import Language.Haskell.TH.All
import Data.List


makeFunctor :: Derivation
makeFunctor = derivation derive "Functor"

derive dat
 | dataArity dat == 0 = []
 | otherwise          = generic_instance (classFor arg) dat [] [funN (fmapFor arg) body]
    where
        arg  = Arg False 1 -- Dervive "Functor1", a.k.a. Functor
        body = map (deriveFunctorCtor dat arg) (dataCtors dat)

-- | Derive Functor over a given argument number for a type
--   return (derived function, required instances)
deriveFunctorCtor :: DataDef -> Arg -> CtorDef -> Clause
deriveFunctorCtor dat arg ctor = sclause lhs rhs
    where
       name = ctorName ctor
       types = ctorTypes ctor
       arity = length types
       args = map return $ take arity ['a'..]
       lhs = [vr "fun", lK name (map vr args)]
       rhs = lK name $ zipWith AppE (map (deriveFunctorType (DeriveInfo arg (ex_args dat))) types)
                                    (map vr args)

-- | What to derive? For what type?
data DeriveInfo = DeriveInfo
	{ fmapPos  :: Arg    -- ^ What kind of functor?
	, dataArgs :: [Name] -- ^ Arguments to the data type
	}

-- | Derive Functor over a given argument number for a type
--   return (derived function, required instances)
deriveFunctorType :: DeriveInfo -> Type -> Exp
deriveFunctorType i (AppT (AppT ArrowT a) b) -- Function type, a -> b
  | isId af && isId bf  =  id'
  | isId af             =  InfixE Nothing   (l0 ".") (Just bf)
  | isId bf             =  InfixE (Just af) (l0 ".") Nothing
  | otherwise           =  LamE [l0 "arg"] $ af .: l0 "arg" .: bf
    where af = deriveFunctorType i{fmapPos = flipCo (fmapPos i)} a
          bf = deriveFunctorType i                               b
deriveFunctorType i (AppT ListT a) -- List type, [a]
  | isId af    =  id'
  | otherwise  =  l1 "map" af
    where af = deriveFunctorType i a
deriveFunctorType i (AppT a b) = deriveFunctorApp i a [b] -- T a b c ...
deriveFunctorType i (ConT n)   = id' -- T
deriveFunctorType i (VarT n)         -- a
  | argPosition i n /= position (fmapPos i) = id'
  | co $ fmapPos i                          = error "tyvar used in covariant position"
  | otherwise                               = l0 "fun"

-- | Derive Functor for an application type, (T a b c)
deriveFunctorApp :: DeriveInfo -> Type -> [Type] -> Exp
deriveFunctorApp i (ForallT _ _ _) _  = error "forall not supported yet in Functor deriving"
deriveFunctorApp i (AppT a b) args = deriveFunctorApp i a (b : args)
deriveFunctorApp i (TupleT n) args -- tuple: (a,b,c)
  | n /= length args    = error "incorrect number of arguments for tuple type constructor"
  | otherwise           = deriveFunctorTuple i args
deriveFunctorApp i (ConT n) args -- apparantly TupleT is not actually used, this hacks around that
  | "(," `isPrefixOf` showName n = deriveFunctorTuple i args
deriveFunctorApp i tycon args -- T a b c
    = foldl (.:) (deriveFunctorType i tycon)
                 (reverse  -- number in reverse, the last argument is fmap, the one before fmap2, etc.
                   [ fmapAp (Arg False n) (deriveFunctorType i a)
                   | (n,a) <- zip [1..] (reverse args)
                   ]
                 )
       -- TODO: if tycon is a VarT, we need some kind of (Functor a) context on the instance

-- | Derive Functor for a tuple type (a,b,...)
--   The result takes the form:
--     \ (t1,t2,...) -> (f t1, g t2, ...)
deriveFunctorTuple i args
  | all isId fArgs = id'
  | otherwise      = LamE [TupP [ vr ("t" ++ show i)
                                | (_,i) <- zip fArgs [1..] ]]
                          (TupE [ AppE a (vr ("t" ++ show i))
                                | (a,i) <- zip fArgs [1..] ])
  where fArgs = map (deriveFunctorType i) args


-- | Is a function the identity function?
isId = (== id')

-- | optimized fmap application
fmapAp arg b
 | isId b    = id'
 | otherwise = l1 (fmapFor arg) b


-- | Derive Functor or CoFunctor over an argument
data Arg = Arg { co :: Bool, position :: Int }

fmapFor  (Arg co i) = (if co then "co" else "") ++ "fmap"    ++ (if i > 1 then show i else "")
classFor (Arg co i) = (if co then "Co" else "") ++ "Functor" ++ (if i > 1 then show i else "")

flipCo (Arg co i) = Arg (not co) i


-- | Position of an argument in the data type
--   In the type  "data X a b c"
--   positions are: a -> 3, b -> 2, c -> 1
argPosition :: DeriveInfo -> Name -> Int
argPosition i nm = case elemIndex nm (dataArgs i) of
    Nothing -> error "impossible: tyvar not in scope"
    Just k  -> length (dataArgs i) - k
