-- | The core module of the Data.Derive system.  This module contains
-- the data types used for communication between the extractors and
-- the derivors.
module Language.Haskell.TH.Data where

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
      ctorFields :: [(Maybe FieldName, RType)] -- ^ The types of the required arguments and the names of the fields.
    } deriving (Eq, Ord)

type FieldName = String
-- | Number of arguments required by this constructor.
ctorArity :: CtorDef -> Int 
ctorArity = length . ctorFields
-- | The types of the required arguments. 
ctorTypes :: CtorDef -> [RType]
ctorTypes = map snd . ctorFields
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
    show (CtorDef name ts) = name ++ " : " ++ show ts

instance Show RType where
    show (RType con [])   = show con
    show (RType con args) = "(" ++ show con ++ concatMap ((" "++) . show) args ++ ")"

instance Show TypeCon where
    show (TypeCon n) = n
    show (TypeArg i) = [chr (ord 'a' + i)]

