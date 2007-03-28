{-# OPTIONS_GHC -fglasgow-exts #-}

-- | The core module of the Data.Derive system.  This module contains
-- the data types used for communication between the extractors and
-- the derivors.
module Data.Derive(
    -- * The data types
    DataDef(..), CtorDef(..),
    Type(..), TypeCon(..),
    Derivation(..),
    -- * Helper functions
    instanceHead
    ) where

import Data.List
import Data.Maybe
import Data.Char

import Language.Haskell.TH.Syntax(Dec)

-- | The type of (algebraic) data declarations.
data DataDef = DataDef {
      dataName :: String,    -- ^ The name of the data type
      dataFree :: Int,       -- ^ The number of arguments to the type
                             -- constructor (eg 3 for @data Foo b c d = ...@)
      dataCtors :: [CtorDef] -- ^ The constructors of the type
    } deriving (Eq, Ord)

-- | The type of individual data constructors.
data CtorDef = CtorDef {
      ctorName :: String, -- ^ The constructor's name.
      ctorArity :: Int,   -- ^ Number of arguments required by this
                          -- constructor.
      ctorTypes :: [Type] -- ^ The types of the required arguments.
    } deriving (Eq, Ord)

-- | A referencing type.  An object of this type refers to some other
-- type.  Presently it is used to specify (components of) the types of
-- constructor arguments.
--
-- @Type@ values are represented in uncurried form, with a principle
-- type constructor followed by a list of zero or more arbitrary type
-- arguments.  The structure of the type guaranteed that the
-- applications are in canononical form.
data Type     = Type  {typeCon :: TypeCon, typeArgs :: [Type] }
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

instance Show Type where
    show (Type con [])   = show con
    show (Type con args) = "(" ++ show con ++ concatMap ((" "++) . show) args ++ ")"

instance Show TypeCon where
    show (TypeCon n) = n
    show (TypeArg i) = [chr (ord 'a' + i)]

-- HELPERS

-- | Construct a string representing an instance head for a class with
-- the property that it is required of all type arguments.
instanceHead :: String -> DataDef -> String
instanceHead cls (DataDef name arity _) =
        "instance " ++
        ['(' | arity > 1] ++
            concat (intersperse ", " [cls ++ " " ++ x | x <- typs]) ++
        [')' | arity > 1] ++
        (if arity > 0 then " => " else "") ++
        cls ++ " " ++
        ['(' | arity > 0] ++
            name ++
            concatMap (' ':) typs ++
        [')' | arity > 0] ++
        " where"
    where
        typs = map (:[]) $ take arity ['a'..]

-- | The type of deriveable classes.
data Derivation = Derivation {
      derivationDeriver :: DataDef -> [Dec], -- ^ The derivation function proper
      derivationName    :: String,           -- ^ The name of the derivation
    }
