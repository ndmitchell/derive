
-- | Derives @Show@.  This is as defined by the Haskell report, except
-- there is no support for infix constructors.  If you attempt to
-- derive @Show@ for a data type with infix constructors, the
-- constructors are handled as if they were prefix constructors, using
-- the @(@/consym/@)@ syntax.
module Data.Derive.Show(makeShow) where

{-

example :: Custom
instance Show a => Show (Sample a) where
    showsPrec p (First) = $(show 0)
    showsPrec p (Second x1 x2) = $(show 1)
    showsPrec p (Third x1) = $(show 2)


test :: Sample
instance Show a => Show (Sample a) where
    showsPrec p First = showString "First"
    showsPrec p (Second x1 x2) = showParen (p > 10) $ showString "Second " . showsPrec 11 x1 . showChar ' ' . showsPrec 11 x2
    showsPrec p (Third x1) = showParen (p > 10) $ showString "Third " . showsPrec 11 x1

test :: Computer
instance Show Computer where
    showsPrec p (Laptop x1 x2) =
        showString "Laptop {weight = " . showsPrec 0 x1 . showString ", speed = " . showsPrec 0 x2 . showChar '}'
    showsPrec p (Desktop x1) =
        showString "Desktop {speed = " . showsPrec 0 x1 . showChar '}'

test :: (:*:)
instance (Show a, Show b) => Show ((:*:) a b) where
    showsPrec p ((:*:) x1 x2) = showParen (p > 10) $ showString "(:*:) " . showsPrec 11 x1 . showChar ' ' . showsPrec 11 x2
-}

import Data.List
import Data.Derive.DSL.HSE
import qualified Language.Haskell as H

-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeShow :: Derivation
makeShow = derivationCustomDSL "Show" custom $
    List [Instance ["Show"] "Show" (List [App "InsDecl" (List [App
    "FunBind" (List [MapCtor (App "Match" (List [App "Ident" (List [
    String "showsPrec"]),List [App "PVar" (List [App "Ident" (List [
    String "p"])]),App "PParen" (List [App "PApp" (List [App "UnQual"
    (List [App "Ident" (List [CtorName])]),MapField (App "PVar" (List
    [App "Ident" (List [Concat (List [String "x",ShowInt FieldIndex])]
    )]))])])],App "Nothing" (List []),App "UnGuardedRhs" (List [App
    "SpliceExp" (List [App "ParenSplice" (List [App "App" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String "show"]
    )])]),App "Lit" (List [App "Int" (List [CtorIndex])])])])])]),App
    "BDecls" (List [List []])]))])])])]
-- GENERATED STOP

-- Left is a literal string, Right is a variable

custom = customSplice splice

splice :: FullDataDecl -> Exp -> Exp
splice d (H.App x (H.Lit (H.Int y))) | x ~= "show" = combine $ compress $
        if fields then customFields c else customPlain c
    where
        fields = any (not . null . fst) (ctorDeclFields c)
        c = dataDeclCtors (snd d) !! fromInteger y

        out (Left [x]) = H.App (var "showChar") (H.Lit $ H.Char x)
        out (Left xs ) = H.App (var "showString") (H.Lit $ H.String xs)
        out (Right x) = apps (var "showsPrec") [H.Lit $ H.Int (fields ? 0 $ 11), var $ 'x' : show x]

        compress (Left x:Left y:z) = compress $ Left (x++y) : z
        compress (x:y) = x : compress y
        compress [] = []

        paren = InfixApp (H.App (var "showParen") (Paren $ InfixApp (var "p") (qvop ">") (H.Lit $ H.Int 10))) (qvop "$")
        combine xs = (fields || or [' ' `notElem` x | Left x <- xs] ? id $ paren) $
                     foldr1 (\x y -> InfixApp x (qvop ".") y) $ map out xs


customPlain :: CtorDecl -> [Either String Int]
customPlain c = intersperse (Left " ") $ Left (ctorDeclName c) : map Right [1..length (ctorDeclFields c)]

customFields :: CtorDecl -> [Either String Int]
customFields c = Left (ctorDeclName c ++ " {") : concat (intersperse [Left ", "] xs) ++ [Left "}"]
    where xs = [[Left (n ++ " = "), Right i] | (i,(n,t)) <- zip [1..] $ ctorDeclFields c]
