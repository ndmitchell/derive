-- Contributed by Tim Newsham <newsham -AT- lava -DOT- net>

{-|
    A pseudo derivation.  Derive a (non-recursive) fold function for 
    the type which takes one function per alternative constructor.  Each
    function takes the same arguments as the constructor and returns
    a value.  When applied to a value the fold function applies the
    function for the matching constructor to the constructor fields.
    This provides a first-class alternative to pattern matching to
    deconstruct the data type.
-}
module Data.Derive.Fold(makeFold) where
{-
test :: Computer

foldComputer :: (Double -> Int -> a) -> (Int -> a) -> Computer -> a
foldComputer f _ (Laptop x1 x2) = f x1 x2
foldComputer _ f (Desktop x1) = f x1

test :: Assoced

foldAssoced :: (typ -> String -> a) -> Assoced typ -> a
foldAssoced f (Assoced x1 x2) = f x1 x2

test :: Either

foldEither :: (a -> c) -> (b -> c) -> Either a b -> c
foldEither f _ (Left x1) = f x1
foldEither _ f (Right x1) = f x1

test :: Bool

foldBool :: a -> a -> Bool -> a
foldBool f _ False = f
foldBool _ f True = f

-}

import Language.Haskell
import Data.Derive.Internal.Derivation
import Data.List
import Data.Generics.Uniplate.DataOnly


makeFold :: Derivation
makeFold = derivationCustom "Fold" $ \(_,d) -> Right $ simplify $ mkFold d


mkFold :: DataDecl -> [Decl ()]
mkFold d | isIdent $ dataDeclName d = [TypeSig () [name n] (foldType d), FunBind () $ zipWith f [0..] $ dataDeclCtors d]
         | otherwise = []
    where
        n = "fold" ++ title (dataDeclName d)
        f i c = Match () (name n) pat (UnGuardedRhs () bod) Nothing
            where pat = replicate i (PWildCard ()) ++ [pVar "f"] ++ replicate (length (dataDeclCtors d) - i - 1) (PWildCard ()) ++
                        [PParen () $ PApp () (qname $ ctorDeclName c) (map pVar vars)]
                  bod = apps (var "f") (map var vars)
                  vars = ['x' : show i | i <- [1..length (ctorDeclFields c)]]


foldType :: DataDecl -> Type ()
foldType d = tyFun $ map f (dataDeclCtors d) ++ [dt, v]
    where
        dt = dataDeclType d
        v = head $ map (tyVar . return) ['a'..] \\ universe dt
        f c = TyParen () $ tyFun $ map snd (ctorDeclFields c) ++ [v]

