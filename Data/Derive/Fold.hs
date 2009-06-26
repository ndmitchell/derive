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
foldComputer f1 f2 (Laptop x1 x2) = f1 x1 x2
foldComputer f1 f2 (Desktop x1) = f2 x1

test :: Assoc

foldAssoc :: (typ -> String -> a) -> Assoc typ -> a
foldAssoc f1 (Assoc x1 x2) = f1 x1 x2

test :: Either

foldEither :: (a -> c) -> (b -> c) -> Either a b -> c
foldEither f1 f2 (Left x1) = f1 x1
foldEither f1 f2 (Right x1) = f2 x1

test :: Bool

foldBool :: a -> a -> Bool -> a
foldBool f1 f2 False = f1
foldBool f1 f2 True = f2

-}

import Language.Haskell
import Data.Derive.Internal.Derivation
import Data.List
import Data.Generics.PlateData


makeFold :: Derivation
makeFold = Derivation "Fold" $ \(_,d) -> Right $ simplify $ mkFold d


mkFold :: DataDecl -> [Decl]
mkFold d = [TypeSig sl [name n] (foldType d), FunBind $ zipWith f funs $ dataDeclCtors d]
    where
        n = "fold" ++ title (dataDeclName d)
        funs = ['f' : show i | i <- [1..length (dataDeclCtors d)]]
        f fun c = Match sl (name n) pat Nothing (UnGuardedRhs bod) (BDecls [])
            where pat = map pVar funs ++ [PParen $ PApp (qname $ ctorDeclName c) (map pVar vars)]
                  bod = app (var fun) (map var vars)
                  vars = ['x' : show i | i <- [1..length (ctorDeclFields c)]]


foldType :: DataDecl -> Type
foldType d = tyFun $ map f (dataDeclCtors d) ++ [dt, v]
    where
        dt = dataDeclType d
        v = head $ map (tyVar . return) ['a'..] \\ universe dt
        f c = TyParen $ tyFun $ map (fromBangType . snd) (ctorDeclFields c) ++ [v]

