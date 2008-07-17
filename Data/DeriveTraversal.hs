{-
    This module is not written/maintained by the usual Data.Derive author.

    MAINTAINER: Twan van Laarhoven 
    EMAIL: "twanvl" ++ "@" ++ "gmail" ++ "." ++ "com"

    Please send all patches to this module to Neil (ndmitchell -at- gmail),
    and CC Twan.
-}

-- NOTE: Cannot be guessed as it relies on type information

-- | Derives 'Functor' and similair classes, as discussed on the Haskell-prime mailing list:
-- <http://www.mail-archive.com/haskell-prime@haskell.org/msg02116.html>.
module Data.DeriveTraversal(
        TraveralType(..), defaultTraversalType,
        traversalDerivation1,
        traversalInstance, traversalInstance1,
        deriveTraversal
    ) where

import Language.Haskell.TH.All
import Data.List
import qualified Data.Set as S
import Control.Monad.Writer
import Control.Applicative

---------------------------------------------------------------------------------
-- Stuff that should be in a library

instance Monoid w => Applicative (Writer w) where
      pure = return
      (<*>) = ap

---------------------------------------------------------------------------------
-- Information datatype, public interface

-- | An expression representing a traversal of a subpart of the data
type Trav = Exp

-- | What kind of traversal are we deriving?
data TraveralType = TraveralType
        { traversalArg    :: Int                     -- ^ On what position are we traversing?
        , traversalCo     :: Bool                    -- ^ covariant?
        , traversalName   :: String                  -- ^ name of the traversal function
        , traversalId     :: Trav                    -- ^ Identity traversal
        , traversalDirect :: Trav                    -- ^ Traversal of 'a'
        , traversalFunc   :: String -> Trav -> Trav  -- ^ Apply the sub-traversal function
        , traversalPlus   :: Trav -> Trav -> Trav    -- ^ Apply two non-identity traversals in sequence
        , traverseArrow   :: Trav -> Trav -> Trav    -- ^ Traverse a function type
        , traverseTuple   :: [Exp] -> Exp            -- ^ Construct a tuple from applied traversals
        , traverseCtor    :: String -> [Exp] -> Exp  -- ^ Construct a data type from applied traversals
        , traverseFunc    :: Pat -> Exp -> Clause    -- ^ Construct a clause of the traversal function
        }

defaultTraversalType = TraveralType
        { traversalArg    = 1
        , traversalCo     = False
        , traversalName   = undefined -- prevent warnings
        , traversalId     = id'
        , traversalDirect = l0 "_f"
        , traversalFunc   = l1
        , traversalPlus   = (.:)
        , traverseArrow   = fail "Cannot derive traversal over function types"
        , traverseTuple   = TupE
        , traverseCtor    = lK
        , traverseFunc    = undefined
        }

data RequiredInstance = RequiredInstance
        { requiredDataArg  :: Name -- ^ What argument of the current data type?
        , requiredPosition :: Int  -- ^ What argument position of that type?
        }
      deriving (Eq, Ord)

-- | Monad that collects required instances
type WithInstances a = Writer (S.Set RequiredInstance) a


---------------------------------------------------------------------------------
-- Deriving traversals


-- | Derivation for a Traversable like class with just 1 method
traversalDerivation1 :: TraveralType -> String -> Derivation
traversalDerivation1 tt nm = derivation (traversalInstance1 tt nm) (className (traversalArg tt))
    where className n = nm ++ (if n > 1 then show n else "")


-- | Instance for a Traversable like class with just 1 method
traversalInstance1 :: TraveralType -> String -> DataDef -> [Dec]
traversalInstance1 tt nm dat = traversalInstance tt nm dat [deriveTraversal tt dat]

-- | Instance for a Traversable like class
traversalInstance :: TraveralType -> String -> DataDef -> [WithInstances Dec] -> [Dec]
traversalInstance tt nameBase dat bodyM
 | dataArity dat == 0 = []
 | otherwise          = [InstanceD ctx head body]
    where
        (body, required) = runWriter (sequence bodyM)
        ctx  = [ lK (className p) (VarT n : vars 's' (p - 1))
               | RequiredInstance n p <- S.toList required
               ]
        vrs  = vars 't' (dataArity dat)
        (vrsBefore,(_:vrsAfter)) = splitAt (length vrs - traversalArg tt) vrs
        className n = nameBase ++ (if n > 1 then show n else "")
        head = lK (className (traversalArg tt)) (lK (dataName dat) vrsBefore : vrsAfter)


-- | Derive a 'traverse' like function
deriveTraversal :: TraveralType -> DataDef -> WithInstances Dec
deriveTraversal tt dat  =  fun
    where
        fun  = funN (traversalNameN tt (traversalArg tt)) <$> body
        args = argPositions dat
        body = mapM (deriveTraversalCtor tt args) (dataCtors dat)


-- | Derive a clause of a 'traverse' like function for a constructor
deriveTraversalCtor :: TraveralType -> ArgPositions -> CtorDef -> WithInstances Clause
deriveTraversalCtor tt ap ctor = do
        tTypes <- mapM (deriveTraversalType tt ap) (ctorTypes ctor)
        return $ traverseFunc tt (ctp ctor 'a')
               $ traverseCtor tt (ctorName ctor) (zipWith AppE tTypes (ctv ctor 'a'))


-- | Derive a traversal for a type
deriveTraversalType :: TraveralType -> ArgPositions -> Type -> WithInstances Trav
deriveTraversalType tt ap (ForallT _ _ _)  = fail "forall not supported in traversal deriving"
deriveTraversalType tt ap (AppT (AppT ArrowT a) b)
                                           = traverseArrow tt <$> deriveTraversalType tt{traversalCo = not $ traversalCo tt} ap a
                                                              <*> deriveTraversalType tt                                     ap b
deriveTraversalType tt ap (AppT a b)       = deriveTraversalApp tt ap a [b] -- T a b c ...
deriveTraversalType tt ap ListT            = return $ traversalId tt -- []
deriveTraversalType tt ap (ConT n)         = return $ traversalId tt -- T
deriveTraversalType tt ap (VarT n) -- a
  | ap n /= traversalArg tt                = return $ traversalId tt
  | traversalCo tt                         = fail "tyvar used in covariant position"
  | otherwise                              = return $ traversalDirect tt


-- | Find all arguments to a type application, then derive a traversal
deriveTraversalApp :: TraveralType -> ArgPositions -> Type -> [Type] -> WithInstances Trav
deriveTraversalApp tt ap (AppT a b) args = deriveTraversalApp tt ap a (b : args)
deriveTraversalApp tt ap tycon args
  | isTupleT tycon = do -- (a,b,c)
         tArgs <- mapM (deriveTraversalType tt ap) args
         return $
           if (all (== traversalId tt) tArgs) then
             traversalId tt
           else
             LamE [TupP                                  (vars 't' (length args))]
                  (traverseTuple tt $ zipWith AppE tArgs (vars 't' (length args)))
  | otherwise = do -- T a b c
         tCon  <- deriveTraversalType tt ap tycon
         tArgs <- mapM (deriveTraversalType tt ap) args
         -- need instances?
         case tycon of
           VarT n | ap n == traversalArg tt -> fail "kind error: type used type constructor"
                  | otherwise               -> tell $ S.fromList
                                                [ RequiredInstance n i
                                                | (t,i) <- zip (reverse tArgs) [1..]
                                                , t /= traversalId tt
                                                ]
           _ -> return ()
         -- combine non-id traversals
         let nonId = [ traverseArg tt i t
                     | (t,i) <- zip (reverse tArgs) [1..]
                     , t /= traversalId tt
                     ]
         return $ case nonId of
           [] -> traversalId tt -- no interesting arguments to type con
           _  -> foldl1 (traversalPlus tt) nonId


-- | Lift a traversal to the argument of a type constructor
traverseArg :: TraveralType -> Int -> Trav -> Trav
traverseArg tt n e   =  traversalFunc tt (traversalNameN tt n) e

traversalNameN :: TraveralType -> Int -> String
traversalNameN tt n  =  traversalName tt ++ (if n > 1 then show n else "")


-- | Information on argument positions
type ArgPositions = Name -> Int

-- | Position of an argument in the data type
--   In the type  "data X a b c"
--   positions are: a -> 3, b -> 2, c -> 1
argPositions :: DataDef -> Name -> Int
argPositions dat = \nm -> case elemIndex nm args of
    Nothing -> error "impossible: tyvar not in scope"
    Just k  -> length args - k
 where args = ex_args dat
