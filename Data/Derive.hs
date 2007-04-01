-- | The core module of the Data.Derive system.  This module contains
-- the data types used for communication between the extractors and
-- the derivors.
module Data.Derive where

import Data.List
import Data.Char

import Language.Haskell.TH.Syntax

-- * The main data types used by Derive

-- | The type of (algebraic) data declarations.
data DataDef = DataDef {
      dataName :: String,    -- ^ The name of the data type
      dataFree :: Int,       -- ^ The number of arguments to the type
                             -- constructor (eg 3 for @data Foo b c d = ...@)
      dataCtors :: [CtorDef] -- ^ The constructors of the type
    } deriving (Eq, Ord)

-- | The type of individual data constructors.
data CtorDef = CtorDef {
      ctorName :: String,  -- ^ The constructor's name.
      ctorArity :: Int,    -- ^ Number of arguments required by this
                           -- constructor.
      ctorTypes :: [RType] -- ^ The types of the required arguments.
    } deriving (Eq, Ord)

-- | A referencing type.  An object of this type refers to some other
-- type.  Presently it is used to specify (components of) the types of
-- constructor arguments.
--
-- @Type@ values are represented in uncurried form, with a principle
-- type constructor followed by a list of zero or more arbitrary type
-- arguments.  The structure of the type guaranteed that the
-- applications are in canononical form.
data RType    = RType {typeCon :: TypeCon, typeArgs :: [RType] }
	deriving (Eq, Ord)

-- | A referencing type which is not itself an application.
data TypeCon = TypeCon String -- ^ A type defined elsewhere, free in
                              -- the data declaration.
             | TypeArg  Int   -- ^ A reference to a type bound by the
                              -- type constructor; the argument to
                              -- @TypeArg@ is the index of the type
                              -- argument, counting from zero at the
                              -- left.
	deriving (Eq, Ord)

instance Show DataDef where
    show (DataDef name arity ctors) = name ++ " #" ++ show arity ++ (if null ctors then "" else " = ") ++ c
        where c = concat $ intersperse " | " $ map show ctors

instance Show CtorDef where
    show (CtorDef name arity ts) = name ++ " #" ++ show arity ++ " : " ++ show ts

instance Show RType where
    show (RType con [])   = show con
    show (RType con args) = "(" ++ show con ++ concatMap ((" "++) . show) args ++ ")"

instance Show TypeCon where
    show (TypeCon n) = n
    show (TypeArg i) = [chr (ord 'a' + i)]

-- | The type of ways to derive classes.
data Derivation = Derivation {
      derivationDeriver :: DataDef -> [Dec], -- ^ The derivation function proper
      derivationName    :: String            -- ^ The name of the derivation
    }

-- * Template Haskell helper functions
--
-- These small short-named functions are intended to make the
-- construction of abstranct syntax trees less tedious.

-- | A simple clause, without where or guards.
sclause :: [Pat] -> Exp -> Clause
sclause pats body = Clause pats (NormalB body) []

-- | A default clause with N arguments.
defclause :: Int -> Exp -> Clause
defclause num = sclause (replicate num WildP)

-- | A simple Val clause
sval :: Pat -> Exp -> Dec
sval pat body = ValD pat (NormalB body) []


-- | The class used to overload lifting operations.  To reduce code
-- duplication, we overload the wrapped constructors (and everything
-- else, but that's irrelevant) to work both in patterns and
-- expressions.
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
      lK nm@(x:_) | isUpper x || x == ':' = foldl AppE (ConE (mkName nm))
      lK nm = foldl AppE (VarE (mkName nm))
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

cons :: Valcon a => a -> a -> a
cons = l2 ":"

box, return' :: Exp -> Exp
box x = cons x nil
return' = l1 "return"

(==:), (&&:), (++:), (>>=:), (>>:), ap' :: Exp -> Exp -> Exp
(==:) = l2 "=="
(&&:) = l2 "&&"
(++:) = l2 "++"
(>>=:) = l2 ">>="
(>>:) = l2 ">>"
ap' = l2 "ap"

case' :: Exp -> [(Pat, Exp)] -> Exp
case' exp alts = CaseE exp [ Match x (NormalB y) [] | (x,y) <- alts ]
(->:) :: String -> Exp -> Exp
(->:) nm bdy = LamE [vr nm] bdy

-- | Build a chain of and-expressions.
and' :: [Exp] -> Exp
and' = foldr (&&:) true

-- | Build a chain of concat-expressions.
concat' :: [Exp] -> Exp
concat' = foldr (++:) nil

-- | Build a chain of monadic actions.
sequ' :: [Exp] -> Exp
sequ' = foldr (>>:) (return' (lit ()))

-- | K-way liftM
liftmk :: Exp -> [Exp] -> Exp
liftmk hd args = foldl ap' (return' hd) args

-- | Build an instance of a class for a data type, using the heuristic
-- that the type is itself required on all type arguments.
simple_instance :: String -> DataDef -> [Dec] -> [Dec]
simple_instance cls (DataDef name arity _) defs = [InstanceD ctx hed defs]
    where
        vars = map (VarT . mkName . ('t':) . show) [1..arity]
        hed = ConT (mkName cls) `AppT` (foldl1 AppT (ConT (mkName name) : vars))
        ctx = map (ConT (mkName cls) `AppT`) vars

-- | Build a fundecl with a string name
funN :: String -> [Clause] -> Dec
funN nam claus = FunD (mkName nam) claus

-- | Make a list of variables, one for each argument to a constructor
ctv :: Valcon a => CtorDef -> Char -> [a]
ctv ctor c = map (vr . (c:) . show) [1 .. ctorArity ctor]

-- | Make a simple pattern to bind a constructor
ctp :: Valcon a => CtorDef -> Char -> a
ctp ctor c = lK (ctorName ctor) (ctv ctor c)

-- | Reference the constructor itself
ctc :: Valcon a => CtorDef -> a
ctc = l0 . ctorName

