{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


-- | These small short-named functions are intended to make the
--   construction of abstranct syntax trees less tedious.
module Language.Haskell.TH.Helper where

import Data.Char

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Data
import Language.Haskell.TH.Compat



-- * Special folds for the guessing


applyWith, foldl1With, foldr1With :: Exp -> [Exp] -> Exp
applyWith  join xs = foldl  AppE join                      xs
foldl1With join xs = foldr1 (\y x -> AppE (AppE join y) x) xs
foldr1With join xs = foldr1 (\y x -> AppE (AppE join x) y) xs



-- * Syntax elements
--

-- | A simple clause, without where or guards.
sclause :: [Pat] -> Exp -> Clause
sclause pats body = Clause pats (NormalB body) []

-- | A default clause with N arguments.
defclause :: Int -> Exp -> Clause
defclause num = sclause (replicate num WildP)

-- | A simple Val clause
sval :: Pat -> Exp -> Dec
sval pat body = ValD pat (NormalB body) []


case' :: Exp -> [(Pat, Exp)] -> Exp
case' exp alts = CaseE exp [ Match x (NormalB y) [] | (x,y) <- alts ]

(->:) :: String -> Exp -> Exp
(->:) nm bdy = LamE [vr nm] bdy

-- | We provide 3 standard instance constructors
--   instance_default requires C for each free type variable
--   instance_none requires no context
--   instance_context requires a given context
instance_none :: String -> DataDef -> [Dec] -> Dec
instance_none = instance_context []

instance_default :: String -> DataDef -> [Dec] -> Dec
instance_default n = instance_context [n] n

instance_context :: [String] -> String -> DataDef -> [Dec] -> Dec
instance_context req cls dat defs = instanceD ctx hed defs
    where
        vrs = vars 't' (dataArity dat)
        hed = l1 cls (lK (dataName dat) vrs)
        ctx = [typeToPred $ l1 r v | r <- req, v <- vrs]


-- | Build an instance of a class for a data type, using the heuristic
-- that the type is itself required on all type arguments.
simple_instance :: String -> DataDef -> [Dec] -> [Dec]
simple_instance cls dat defs = [instance_default cls dat defs]

-- | Build an instance of a class for a data type, using the class at the given types
generic_instance :: String -> DataDef -> [Type] -> [Dec] -> [Dec]
generic_instance cls dat ctxTypes defs = [instanceD ctx hed defs]
    where
        vrs = vars 't' (dataArity dat)
        hed = l1 cls (lK (dataName dat) vrs)
        ctx = map (typeToPred . l1 cls) ctxTypes

-- | Build a type signature declaration with a string name
sigN :: String -> Type -> Dec
sigN nam ty = SigD (mkName nam) ty

-- | Build a fundecl with a string name
funN :: String -> [Clause] -> Dec
funN nam claus = FunD (mkName nam) claus

-- * Pattern vs Value abstraction

class Eq nm => NameLike nm where
  toName :: nm -> Name
instance NameLike Name   where toName = id
instance NameLike String where toName = mkName

-- | The class used to overload lifting operations.  To reduce code
-- duplication, we overload the wrapped constructors (and everything
-- else, but that's irrelevant) to work in patterns, expressions, and
-- types.
class Valcon a where
      -- | Build an application node, with a name for a head and a
      -- provided list of arguments.
      lK :: NameLike nm => nm -> [a] -> a
      -- | Reference a named variable.
      vr :: NameLike nm => nm -> a
      -- | Lift a TH 'Lit'
      raw_lit :: Lit -> a
      -- | Tupling
      tup :: [a] -> a
      -- | Listing
      lst :: [a] -> a
instance Valcon Exp where
      lK nm ys = let name = toName nm in case (nameBase name, ys) of
        ("[]", []) -> ConE name
        ("[]", xs) -> lst xs
        ((x:_), args)  | isUpper x || x == ':' -> foldl AppE (ConE name) args
        ((x:_), [a,b]) | isOper x -> InfixE (Just a) (VarE name) (Just b)
         where isOper x = not (isAlpha x || x == '_')
        (nm,     args) -> foldl AppE (VarE name) args

      vr = VarE . toName
      raw_lit = LitE
      tup = TupE
      lst = ListE
instance Valcon Pat where
      lK = ConP . toName
      vr = VarP . toName
      raw_lit = LitP
      tup = TupP
      lst = ListP
instance Valcon Type where
      lK nm = foldl AppT (if bNm == "[]" then ListT else ConT (mkName bNm))
        where bNm = nameBase (toName nm)
      vr = VarT . toName
      raw_lit = error "raw_lit @ Type"

      -- XXX work around bug in GHC < 6.10
      -- (see http://hackage.haskell.org/trac/ghc/ticket/2358 for details)
      tup [t] = t
      tup ts  = foldl AppT (TupleT (length ts)) ts

      lst = error "lst @ Type"

-- | Build an application node without a given head
app :: Exp -> [Exp] -> Exp
app root args = foldl AppE root args


-- | This class is used to overload literal construction based on the
-- type of the literal.
class LitC a where
      lit :: Valcon p => a -> p
instance LitC Integer where
      lit = raw_lit . IntegerL
instance LitC Char where
      lit = raw_lit . CharL
instance LitC a => LitC [a] where
      lit = lst . map lit
instance (LitC a, LitC b) => LitC (a,b) where
      lit (x,y) = tup [lit x, lit y]
instance (LitC a, LitC b, LitC c) => LitC (a,b,c) where
      lit (x,y,z) = tup [lit x, lit y, lit z]
instance LitC () where
      lit () = tup []


-- * Constructor abstraction

dataVars :: DataDef -> [Type]
dataVars dat = take (dataArity dat) $ map (VarT . mkName . return) ['a'..]

-- | Common pattern: list of a familiy of variables
vars :: Valcon a => Char -> Int -> [a]
vars c n = map (vrn c) [1 .. n]

-- | Variable based on a letter + number
vrn :: Valcon a => Char -> Int -> a
vrn c n = vr (c : show n)

-- | Make a list of variables, one for each argument to a constructor
ctv :: Valcon a => CtorDef -> Char -> [a]
ctv ctor c = vars c (ctorArity ctor)

-- | Make a simple pattern to bind a constructor
ctp :: Valcon a => CtorDef -> Char -> a
ctp ctor c = lK (ctorName ctor) (ctv ctor c)

-- | Reference the constructor itself
ctc :: Valcon a => CtorDef -> a
ctc = l0 . ctorName


-- * Lift a constructor over a fixed number of arguments.

l0 :: (NameLike nm, Valcon a) => nm -> a
l1 :: (NameLike nm, Valcon a) => nm -> a -> a
l2 :: (NameLike nm, Valcon a) => nm -> a -> a -> a
l0 s     = lK s []
l1 s a   = lK s [a]
l2 s a b = lK s [a,b]

-- * Pre-lifted versions of common operations
true, false, nil :: Valcon a => a
hNil', hZero' :: Type
true = l0 "True"
false = l0 "False"
nil = l0 "[]"
unit = lit ()
hNil' = l0 "HNil"
hZero' = l0 "HZero"
id' = l0 "id"

cons :: Valcon a => a -> a -> a
cons = l2 ":"

box :: Valcon a => a -> a
return', const' :: Exp -> Exp
hSucc' :: Type -> Type
box x = cons x nil
return' = l1 "return"
const' = l1 "const"
hSucc' = l1 "HSucc"

(==:), (&&:), (++:), (>>=:), (>>:), (.:), ap', (>:) :: Exp -> Exp -> Exp
hCons' :: Type -> Type -> Type
(==:) = l2 "=="
(&&:) = l2 "&&"
(++:) = l2 "++"
(>>=:) = l2 ">>="
(>>:) = l2 ">>"
(.:) = l2 "."
(>:) = l2 ">"
ap' = l2 "ap"
hCons' = l2 "HCons"

-- | Build a chain of expressions, with an appropriate terminal
--   sequence__ does not require a unit at the end (all others are optimised automatically)
(&&::), (++::), (>>::), sequence__, (.::) :: [Exp] -> Exp
(&&::)  = foldr (&&:) true
(++::) = foldr (++:) nil
(>>::) = foldr (>>:) (return' unit)
(.::) = foldr (.:) id'

sequence__ [] = return' unit
sequence__ xs = foldr1 (>>:) xs


-- | K-way liftM
liftmk :: Exp -> [Exp] -> Exp
liftmk hd args = foldl ap' (return' hd) args
