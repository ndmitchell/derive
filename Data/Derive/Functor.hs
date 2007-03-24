
module Data.Derive.Functor(derive) where

import Data.Derive
import Data.List
import qualified Data.Set as Set

derive :: DataDef -> [String]
derive dat@(DataDef name arity ctors)
 | arity == 0 = []
 | otherwise  = instanceHeadFunctor i dat (Set.unions reqs) : codes
    where
        i = Arg 1 -- TODO : make a parameter
        (codes, reqs) = unzip $ map (deriveFunctorCtor i) ctors

-- | Derived function
data Function
	= Fun { code :: String, requires :: Set.Set (Arg, Type) }
	| Nop
	deriving (Eq)

-- | Derive Functor or CoFunctor over an argument
data Arg = Arg { count :: Int } | CoArg { count :: Int }
	deriving (Eq, Ord, Show)

co (Arg   i) = CoArg i
co (CoArg i) = Arg i

fmapFor (Arg   1) = "fmap"
fmapFor (Arg   i) = "fmap" ++ show i
fmapFor (CoArg 1) = "cofmap"
fmapFor (CoArg i) = "cofmap" ++ show i
classFor (Arg   1) = "Functor"
classFor (Arg   i) = "Functor" ++ show i
classFor (CoArg 1) = "CoFunctor"
classFor (CoArg i) = "CoFunctor" ++ show i

-- | Create the string "Functor3 (Type a) c d" for a type 'Type a b c d'
instanceFor :: Arg -> Type -> String
instanceFor i t = classFor i ++ " " ++
                  show (dropArgs (count i) t) ++
                  concatMap ((' ':) . show) (takeArgs (count i - 1) t)
   where
      dropArgs n (Type c as) = Type c $ reverse $ drop n $ reverse as
      takeArgs n (Type c as) =          reverse $ take n $ reverse as

-- | Instance head for FunctorN
instanceHeadFunctor :: Arg -> DataDef -> Set.Set (Arg, Type) -> String
instanceHeadFunctor i (DataDef name arity _) reqs
   = "instance " ++ context ++ instanceFor i typ ++ " where"
   where
       typ = Type (TypeCon name) (map (flip Type [] . TypeCon . return) (take arity ['a'..]))
       -- remove the instance we are deriving now
       reqs' = Set.filter (\(a, Type c _) -> a /= i || c /= TypeCon name) reqs
       context
        | Set.null reqs' = ""
        | otherwise      = "(" ++ concatMap (uncurry instanceFor) (Set.toList reqs') ++ ") => "

-- | Derive Functor over a given argument number for a type
--   return (derived function, required instances)
deriveFunctorCtor :: Arg -> CtorDef -> (String, Set.Set (Arg, Type))
deriveFunctorCtor i (CtorDef name arity types) = (lhs ++ " = " ++ rhs, Set.unions reqs)
    where
       args = map return $ take arity ['a'..]
       lhs
         | arity == 0 = "    " ++ fmapFor i ++ " fun "  ++ name
         | otherwise  = "    " ++ fmapFor i ++ " fun (" ++ name ++ concatMap (' ':) args ++ ")"
       rhs = name ++ concatMap (' ':) (zipWith ($) fts args)
       (fts, reqs) = unzip $ map (toCode . deriveFunctorType i) types
       toCode Nop       = (id,                      Set.empty)
       toCode (Fun c r) = (\arg -> "(" ++ c ++ " " ++ arg ++ ")", r)

-- | Derive Functor over a given argument number for a type
--   return (derived function, required instances)
deriveFunctorType :: Arg -> Type -> Function
deriveFunctorType i (Type (TypeCon "->") [a,b]) -- a -> b
  | af == Nop && bf == Nop  =  Nop
  | af == Nop               =  Fun ("(. " ++ code bf ++   ")") (requires bf)
  | bf == Nop               =  Fun ("("   ++ code af ++ " .)") (requires af)
  | otherwise               =  Fun ("\arg -> (" ++ code af ++ ") . arg . (" ++ code bf ++ ")") (requires af `Set.union` requires bf)
    where af = deriveFunctorType (co i) a
          bf = deriveFunctorType i      b
deriveFunctorType i typ@(Type tycon args)
    = foldl  composeAp  (deriveFunctorCon i tycon)
                        (zipWith (fmapAp typ) (map Arg [0..]) (map (deriveFunctorType i) args))

composeAp a         Nop       = a
composeAp Nop       b         = b
composeAp (Fun a b) (Fun c d) = Fun (a ++ " . " ++ c) (Set.union b d)

fmapAp t i  Nop           = Nop
fmapAp t i (Fun code req) = Fun (fmapFor i ++ " (" ++ code ++ ")") (Set.insert (i,t) req)

-- | Derive Functor over a given argument number for a type constructor
deriveFunctorCon :: Arg -> TypeCon -> Function
deriveFunctorCon (Arg i)   (TypeArg j) | i == j = Fun "fun" Set.empty
deriveFunctorCon (CoArg i) (TypeArg j) | i == j = error "argument used in contravariant position"
deriveFunctorCon _         _                    = Nop
