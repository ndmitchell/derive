{-# LANGUAGE PatternGuards #-}

{-# OPTIONS_GHC -Wwarn #-}
{-
Otherwise I get:

src/Language/Haskell/TH/Peephole.hs:64:1: warning:
    Pattern match checker exceeded (2000000) iterations in
    an equation for ‘peep’. (Use -fmax-pmcheck-iterations=n
    to set the maximun number of iterations to n)

Seriously. Your warning checker is crap. My code is fine.
Don't produce warnings about code I can't possibly fix.
Especially not by default.
-}

module Language.Haskell.TH.Peephole(peephole, replaceVar, replaceVars) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Helper
import Data.Generics
import Data.Maybe
import Data.List
import Debug.Trace

traceMode = False


peephole :: Data a => a -> a
peephole = everywhere (mkT peep) . everywhere (mkT peepPat)



-- find a given string, and replace it with a particular expression
-- must succeed, so crashes readily (deliberately!)
replaceVars :: [(Name,Exp)] -> Exp -> Exp
replaceVars rep orig = fExp orig
    where
        fExp x = case x of
            VarE y -> fromMaybe x $ lookup y rep
            ConE _ -> x
            LitE _ -> x
            AppE x y -> AppE (fExp x) (fExp y)
            CaseE x y -> CaseE (fExp x) (map fMatch y)
            TupE xs -> TupE (map fExp xs)
            ListE xs -> ListE (map fExp xs)
            LamE x y -> LamE x (fPat x y)
            _ | null $ map fst rep `intersect` getNames x -> x
            _ -> error $ "replaceVar: " ++ show x

        getNames x = everything (++) ([] `mkQ` f) x
            where
                f :: Name -> [Name]
                f x = [x]

        fMatch o@(Match pat (NormalB bod) []) =
            Match pat (NormalB $ fPat [pat] bod) []

        -- given these pattern have come into scope
        -- continue matching on the rest
        fPat :: [Pat] -> Exp -> Exp
        fPat pat = replaceVars (filter ((`notElem` used) . fst) rep)
            where used = concatMap usedPats pat

        usedPats x = everything (++) ([] `mkQ` f) x
            where
                f (VarP x) = [x]
                f _ = []


replaceVar :: Name -> Exp -> Exp -> Exp
replaceVar name with = replaceVars [(name,with)]



-- based on the rewrite combinator in Play
peep :: Exp -> Exp
peep (ListE xs)
    | not (null xs) && all (isJust . fromLitChar) xs =
      peep $ LitE $ StringL $ map (fromJust . fromLitChar) xs
    where
        fromLitChar (LitE (CharL x)) = Just x
        fromLitChar _ = Nothing

peep (AppE x y)
    | x ~= "id" = y

peep (AppE (AppE op x) y)
    | Just res <- peepBin op x y = res

peep (InfixE (Just x) op (Just y))
    | Just res <- peepBin op x y = res

peep (LamE [] x) = x

peep (LamE [VarP x] (VarE y))
    | x == y = l0 "id"

peep (DoE [NoBindS x]) = x

peep x@(ConE _)
    | x ~= "[]" = ListE []

peep (AppE (AppE cons x) nil)
    | cons ~= ":" && nil ~= "[]" = ListE [x]

peep (DoE [BindS (VarP p) (AppE ret (LitE val)),NoBindS e])
    | ret ~= "return" = peep $ replaceVar p (LitE val) e

peep (LamE [TupP [VarP x, VarP y]] (VarE z))
    | x == z = l0 "fst"
    | y == z = l0 "snd"

peep (AppE (LamE (VarP x:xs) y) z)
    | simple z
    = peep $ LamE xs (replaceVar x z y)

peep (AppE (AppE bind (AppE ret x)) y)
    | bind ~= ">>=" && ret ~= "return" = peep $ AppE y x

peep (InfixE (Just (AppE ret x)) bind (Just y))
    | bind ~= ">>=" && ret ~= "return" = peep $ AppE y x

peep (InfixE (Just (AppE pure x)) ap y)
    | ap ~= "<*>" && pure ~= "pure" = peep $ InfixE (Just x) (l0 "<$>") y

peep (InfixE (Just x) fmap (Just (AppE pure y)))
    | fmap ~= "<$>" && pure ~= "pure" = peep $ AppE pure (peep $ AppE x y)

peep (AppE append (ListE [x]))
    | append ~= "++" = peep $ AppE (l0 ":") x

peep (InfixE (Just (ListE [x])) append y)
    | append ~= "++" = peep $ InfixE (Just x) (l0 ":") y

peep (InfixE (Just x) cons (Just (ListE xs)))
    | cons ~= ":" = peep $ ListE (x:xs)

peep (AppE (AppE (AppE comp f) g) x)
    | comp ~= "."  = peep $ AppE f (peep $ AppE g x)
peep (AppE (InfixE (Just f) comp (Just g)) x)
    | comp ~= "."  = peep $ AppE f (peep $ AppE g x)

peep (AppE (AppE (AppE flip f) x) y)
    | flip ~= "flip"  = peep $ AppE (AppE f y) x

peep (AppE (InfixE (Just x) op Nothing) y) = peep $ InfixE (Just x) op (Just y)
peep (AppE (InfixE Nothing op (Just y)) x) = peep $ InfixE (Just x) op (Just y)

peep (AppE f (LamE x (ListE [y])))
    | f ~= "concatMap" = peep $ AppE (l0 "map") (peep $ LamE x y)

peep (AppE f (ListE xs))
    | f ~= "head" && not (null xs) = head xs
    | f ~= "reverse" = ListE $ reverse xs

peep (AppE f (TupE [x,y]))
    | f ~= "choose" && x == y = peep $ AppE (VarE (mkName "return")) x

peep (AppE (AppE sq o@(AppE rnf x)) (TupE []))
    | sq ~= "seq" && rnf ~= "rnf" = o

peep (CaseE (LitE x) (Match (LitP y) (NormalB z) [] : _))
    | x == y = z

peep (AppE len (ListE xs))
    | len ~= "length" = LitE $ IntegerL $ toInteger $ length xs

peep (TupE [x]) = x

peep (AppE (LamE [pat] x) e) = CaseE e [Match pat (NormalB x) []]

peep (AppE (CaseE e [Match p (NormalB x) []]) y)
       = CaseE e [Match p (NormalB $ peep $ AppE x y) []]

-- allow easy flip to tracing mode
peep x | traceMode = trace (show x) x
peep x = x


peepPat :: Pat -> Pat
peepPat (ListP xs)
    | all (\x -> case x of LitP (CharL _) -> True
                           _ -> False) xs =
      LitP $ StringL $ map (\(LitP (CharL x)) -> x) xs

peepPat x = x

peepBin :: Exp -> Exp -> Exp -> Maybe Exp
peepBin op x y
    | op ~= "." && x ~= "id" = Just y
    | op ~= "." && y ~= "id" = Just x
    | op ~= "&&" && y ~= "True" = Just x
    | op ~= "const" = Just x
    | op ~= "map" && x ~= "id" = Just y
    | op ~= "++" && x ~= "[]" = Just y
    | op ~= "++" && y ~= "[]" = Just x
    | op ~= "." && y ~= "id" = Just x
    | op ~= ">>" && x ~= "return" && y == TupE [] = Just $ l0 "id"
    | op ~= "$" = Just $ peep $ AppE x y

peepBin op (LitE (StringL x)) (LitE (StringL y))
    | op ~= "++" = Just $ LitE $ StringL (x++y)

peepBin _ _ _ = Nothing


(VarE f) ~= x = show f == x
(ConE f) ~= x = show f == x
(ListE []) ~= "[]" = True
_ ~= _ = False


simple (VarE _) = True
simple (LitE _) = True
simple _ = False
