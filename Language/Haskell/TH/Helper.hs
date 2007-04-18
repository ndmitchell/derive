-- | These small short-named functions are intended to make the
--   construction of abstranct syntax trees less tedious.
module Language.Haskell.TH.Helper where

import Data.List
import Data.Char

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Data



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
instance_context req cls dat defs = InstanceD ctx hed defs
    where
        vrs = vars (dataArity dat)
        hed = l1 cls (lK (dataName dat) vars)
        ctx = [l1 r v | r <- req, v <- vars]


-- | Build an instance of a class for a data type, using the heuristic
-- that the type is itself required on all type arguments.
simple_instance :: String -> DataDef -> [Dec] -> [Dec]
simple_instance cls dat defs = [instance_default cls dat defs]

-- | Build an instance of a class for a data type, using the class at the given types
generic_instance :: String -> DataDef -> [Type] -> [Dec] -> [Dec]
generic_instance cls dat ctxTypes defs = [InstanceD ctx hed defs]
    where
        vrs = vars (dataArity dat)
        hed = l1 cls (lK (dataName dat) vars)
        ctx = map (l1 cls) ctxTypes

-- | Build a fundecl with a string name
funN :: String -> [Clause] -> Dec
funN nam claus = FunD (mkName nam) claus

-- * Pattern vs Value abstraction

-- | The class used to overload lifting operations.  To reduce code
-- duplication, we overload the wrapped constructors (and everything
-- else, but that's irrelevant) to work in patterns, expressions, and
-- types.
class Valcon a where
      -- | Build an application node, with a name for a head and a
      -- provided list of arguments.
      lK :: String -> [a] -> a
      -- | Reference a named variable.
      vr :: String -> a
      -- | Lift a TH 'Lit'
      raw_lit :: Lit -> a
      -- | Tupling
      tup :: [a] -> a
      -- | Listing
      lst :: [a] -> a
instance Valcon Exp where
      lK nm@(x:_) args | isUpper x || x == ':' = foldl AppE (ConE (mkName nm)) args
      lK nm@(x:_) [a,b] | isOper x = InfixE (Just a) (VarE (mkName nm)) (Just b)
         where isOper x = not (isAlpha x || x == '_')
      lK nm       args = foldl AppE (VarE (mkName nm)) args
      vr = VarE . mkName
      raw_lit = LitE
      tup = TupE
      lst = ListE
instance Valcon Pat where
      lK = ConP . mkName
      vr = VarP . mkName
      raw_lit = LitP
      tup = TupP
      lst = ListP
instance Valcon Type where
      lK nm = foldl AppT (if nm == "[]" then ListT else ConT (mkName nm))
      vr = VarT . mkName
      raw_lit = error "raw_lit @ Type"
      tup l = foldl AppT (TupleT (length l)) l
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

-- | Common pattern: list of a familiy of variables
vars :: Valcon a => Char -> Int -> [a]
vars c n = map (vr . (c:) . show) [1 .. n]

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

l0 :: Valcon a => String -> a
l1 :: Valcon a => String -> a -> a
l2 :: Valcon a => String -> a -> a -> a
l0 s     = lK s []
l1 s a   = lK s [a]
l2 s a b = lK s [a,b]

-- * Pre-lifted versions of common operations
true, false, nil :: Valcon a => a
true = l0 "True"
false = l0 "False"
nil = l0 "[]"
unit = lit ()

cons :: Valcon a => a -> a -> a
cons = l2 ":"

box, return', const' :: Exp -> Exp
box x = cons x nil
return' = l1 "return"
const' = l1 "const"

(==:), (&&:), (++:), (>>=:), (>>:), (.:), ap' :: Exp -> Exp -> Exp
(==:) = l2 "=="
(&&:) = l2 "&&"
(++:) = l2 "++"
(>>=:) = l2 ">>="
(>>:) = l2 ">>"
(.:) = l2 "."
ap' = l2 "ap"

-- | Build a chain of expressions, with an appropriate terminal
--   sequence__ does not require a unit at the end (all others are optimised automatically)
and_, concat_, sequence_, sequence__ :: [Exp] -> Exp
and_  = foldr (&&:) true
concat_ = foldr (++:) nil
sequence_ = foldr (>>:) (return' unit)

sequence__ [] = return' unit
sequence__ xs = foldr1 (>>:) xs


-- | K-way liftM
liftmk :: Exp -> [Exp] -> Exp
liftmk hd args = foldl ap' (return' hd) args
