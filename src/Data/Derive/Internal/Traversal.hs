{-# LANGUAGE CPP #-}
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
module Data.Derive.Internal.Traversal(
        TraveralType(..), defaultTraversalType,
        traversalDerivation1,
        traversalInstance, traversalInstance1,
        deriveTraversal
    ) where

import Language.Haskell
import Data.Derive.Internal.Derivation
import Data.List
import qualified Data.Set as S
import Control.Monad.Trans.Writer
import Control.Applicative
import Data.Generics.Uniplate.DataOnly
import Data.Maybe
import Prelude

---------------------------------------------------------------------------------
-- Information datatype, public interface

-- | An expression representing a traversal of a subpart of the data
type Trav = Exp ()

-- | What kind of traversal are we deriving?
data TraveralType = TraveralType
        { traversalArg    :: Int                           -- ^ On what position are we traversing?
        , traversalCo     :: Bool                          -- ^ covariant?
        , traversalName   :: QName ()                      -- ^ name of the traversal function
        , traversalId     :: Trav                          -- ^ Identity traversal
        , traversalDirect :: Trav                          -- ^ Traversal of 'a'
        , traversalFunc   :: QName () -> Trav -> Trav      -- ^ Apply the sub-traversal function
        , traversalPlus   :: Trav -> Trav -> Trav          -- ^ Apply two non-identity traversals in sequence
        , traverseArrow   :: Maybe (Trav -> Trav -> Trav)  -- ^ Traverse a function type
        , traverseTuple   :: [Exp ()] -> Exp ()            -- ^ Construct a tuple from applied traversals
        , traverseCtor    :: String -> [Exp ()] -> Exp ()  -- ^ Construct a data type from applied traversals
        , traverseFunc    :: Pat () -> Exp () -> Match ()  -- ^ Construct a clause of the traversal function
        }

defaultTraversalType = TraveralType
        { traversalArg    = 1
        , traversalCo     = False
        , traversalName   = undefined -- prevent warnings
        , traversalId     = var "id"
        , traversalDirect = var "_f"
        , traversalFunc   = \x y -> appP (Var () x) y
        , traversalPlus   = \x y -> apps (Con () $ Special () (Cons ())) [paren x, paren y]
        , traverseArrow   = Nothing
        , traverseTuple   = Tuple () Boxed
        , traverseCtor    = \x y -> apps (con x) (map paren y)
        , traverseFunc    = undefined
        }

data RequiredInstance = RequiredInstance
        { _requiredDataArg  :: String -- ^ What argument of the current data type?
        , _requiredPosition :: Int    -- ^ What argument position of that type?
        }
      deriving (Eq, Ord)

-- | Monad that collects required instances
type WithInstances a = Writer (S.Set RequiredInstance) a


vars f c n = [f $ c : show i | i <- [1..n]]


---------------------------------------------------------------------------------
-- Deriving traversals


-- | Derivation for a Traversable like class with just 1 method
traversalDerivation1 :: TraveralType -> String -> Derivation
traversalDerivation1 tt nm = derivationCustom (className $ traversalArg tt) (traversalInstance1 tt nm)
    where className n = nm ++ (if n > 1 then show n else "")


-- | Instance for a Traversable like class with just 1 method
traversalInstance1 :: TraveralType -> String -> FullDataDecl -> Either String [Decl ()]
traversalInstance1 tt nm (_,dat)
    | isNothing (traverseArrow tt) && any isTyFun (universeBi dat) = Left $ "Can't derive " ++ prettyPrint (traversalName tt) ++ " for types with arrow"
    | dataDeclArity dat == 0 = Left "Cannot derive class for data type arity == 0"
    | otherwise = Right $ traversalInstance tt nm dat [deriveTraversal tt dat]


-- InstDecl SrcLoc (Maybe Overlap) [TyVarBind] Context QName [Type] [InstDecl]
-- InstDecl l (Maybe (Overlap l)) (InstRule l) (Maybe [InstDecl l])

-- | Instance for a Traversable like class
traversalInstance :: TraveralType -> String -> DataDecl -> [WithInstances (Decl ())] -> [Decl ()]
traversalInstance tt nameBase dat bodyM = -- [simplify $ InstDecl () Nothing [] ctx nam args (map InsDecl body)]
      [ simplify $ InstDecl () Nothing instRule (Just $ map (InsDecl ()) body) ]
    where
        instRule = IRule () Nothing (Just ctx) instHead
        instHead = foldr (flip (IHApp ())) (IHCon () nam) args
        (body, required) = runWriter (sequence bodyM)
        ctx  = CxTuple ()
               [ ClassA () (qname $ className p) (tyVar n : vars tyVar 's' (p - 1))
               | RequiredInstance n p <- S.toList required
               ]
        vrs  = vars tyVar 't' (dataDeclArity dat)
        (vrsBefore,_:vrsAfter) = splitAt (length vrs - traversalArg tt) vrs
        className n = nameBase ++ (if n > 1 then show n else "")
        nam = qname (className (traversalArg tt))
        args = TyParen () (tyApps (tyCon $ dataDeclName dat) vrsBefore) : vrsAfter

-- Match SrcLoc Name [Pat] (Maybe Type) Rhs (Maybe Binds)
-- Match l (Name l) [Pat l] (Rhs l) (Maybe (Binds l))
-- | Derive a 'traverse' like function
deriveTraversal :: TraveralType -> DataDecl -> WithInstances (Decl ())
deriveTraversal tt dat = fun
    where
        fun  = (\xs -> FunBind () [Match () nam a b c | Match () _ a b c <- xs]) <$> body
        args = argPositions dat
        nam = unqual $ traversalNameN tt $ traversalArg tt
        body = mapM (deriveTraversalCtor tt args) (dataDeclCtors dat)

        unqual (Qual () _ x) = x
        unqual (UnQual () x) = x


-- | Derive a clause of a 'traverse' like function for a constructor
deriveTraversalCtor :: TraveralType -> ArgPositions -> CtorDecl -> WithInstances (Match ())
deriveTraversalCtor tt ap ctor = do
    let nam = ctorDeclName ctor
        arity = ctorDeclArity ctor
    tTypes <- mapM (deriveTraversalType tt ap) (map snd $ ctorDeclFields ctor)
    return $ traverseFunc tt (PParen () $ PApp () (qname nam) (vars pVar 'a' arity))
           $ traverseCtor tt nam (zipWith (App ()) tTypes (vars var 'a' arity))



-- | Derive a traversal for a type
deriveTraversalType :: TraveralType -> ArgPositions -> Type () -> WithInstances Trav
deriveTraversalType tt ap (TyParen () x) = deriveTraversalType tt ap x
deriveTraversalType tt ap TyForall{}  = fail "forall not supported in traversal deriving"
deriveTraversalType tt ap (TyFun () a b)
                                           = fromJust (traverseArrow tt)
                                                 <$> deriveTraversalType tt{traversalCo = not $ traversalCo tt} ap a
                                                 <*> deriveTraversalType tt                                     ap b
deriveTraversalType tt ap (TyApp () a b)      = deriveTraversalApp tt ap a [b] -- T a b c ...
deriveTraversalType tt ap (TyList () a)       = deriveTraversalType tt ap $ TyApp () (TyCon () $ Special () $ ListCon ()) a
deriveTraversalType tt ap (TyTuple () b a)    = deriveTraversalType tt ap $ tyApps (TyCon () $ Special () $ TupleCon () b $ length a) a
deriveTraversalType tt ap (TyCon () n)        = return $ traversalId tt -- T
deriveTraversalType tt ap (TyVar () (Ident () n)) -- a
  | ap n /= traversalArg tt                = return $ traversalId tt
  | traversalCo tt                         = fail "tyvar used in covariant position"
  | otherwise                              = return $ traversalDirect tt


-- | Find all arguments to a type application, then derive a traversal
deriveTraversalApp :: TraveralType -> ArgPositions -> Type () -> [Type ()] -> WithInstances Trav
deriveTraversalApp tt ap (TyApp () a b) args = deriveTraversalApp tt ap a (b : args)
deriveTraversalApp tt ap tycon@TyTuple{} args = do -- (a,b,c)
         tArgs <- mapM (deriveTraversalType tt ap) args
         return $
           if (all (== traversalId tt) tArgs) then
             traversalId tt
           else
             Lambda () [PTuple () Boxed (vars pVar 't' (length args))]
                  (traverseTuple tt $ zipWith (App ()) tArgs (vars var 't' (length args)))
deriveTraversalApp tt ap tycon args = do -- T a b c
         tCon  <- deriveTraversalType tt ap tycon
         tArgs <- mapM (deriveTraversalType tt ap) args
         -- need instances?
         case tycon of
           TyVar () (Ident () n) | ap n == traversalArg tt -> fail "kind error: type used type constructor"
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

traversalNameN :: TraveralType -> Int -> QName ()
traversalNameN tt n | n <= 1    = nm
                    | otherwise = nm `f` (if n > 1 then show n else "")
  where nm = traversalName tt
        f (Qual () m x) y = Qual () m $ x `g` y
        f (UnQual () x) y = UnQual () $ x `g` y
        g (Ident () x) y = Ident () $ x ++ y

-- | Information on argument positions
type ArgPositions = String -> Int

-- | Position of an argument in the data type
--   In the type  "data X a b c"
--   positions are: a -> 3, b -> 2, c -> 1
argPositions :: DataDecl -> String -> Int
argPositions dat = \nm -> case elemIndex nm args of
    Nothing -> error "impossible: tyvar not in scope"
    Just k  -> length args - k
 where args = dataDeclVars dat
