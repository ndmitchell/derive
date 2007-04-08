
module Language.Haskell.TH.Peephole(peepholeDecs) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Helper
import Language.Haskell.TH.SYB
import Data.Generics
import Debug.Trace

traceMode = False


peepholeDecs :: [Dec] -> [Dec]
peepholeDecs x = everywhere (mkT peep) x



-- find a given string, and replace it with a particular expression
-- must succeed, so crashes readily (deliberately!)
replaceVar :: Name -> Exp -> Exp -> Exp
replaceVar name rep orig = fExp orig
    where
        fExp x = case x of
            VarE y | y == name -> rep
                   | otherwise -> x
            ConE _ -> x
            LitE _ -> x
            AppE x y -> AppE (fExp x) (fExp y)
            CaseE x y -> CaseE (fExp x) (map fMatch y)
            TupE xs -> TupE (map fExp xs)
            _ -> error $ "replaceVar: " ++ show x

        fMatch o@(Match pat (NormalB bod) []) =
            if name `elem` patFree pat then o else Match pat (NormalB (fExp bod)) []


        patFree x = case x of
            LitP _ -> []
            _ -> error $ "patFree: " ++ show x



-- based on the rewrite combinator in Play
peep :: Exp -> Exp
peep (AppE x y)
    | x ~= "id" = y

peep (AppE (AppE x y) z)
    | x ~= "const" = y
    | x ~= "map" && y ~= "id" = z
    | x ~= "++" && y ~= "[]" = z
    | x ~= "++" && z ~= "[]" = y
    | x ~= "." && z ~= "id" = y

peep (AppE bind (AppE ret (TupE [])))
    | bind ~= ">>" && ret ~= "return" = l0 "id"

peep (LamE [] x) = x

peep (LamE [VarP x] (VarE y))
    | x == y = l0 "id"

peep (LamE [ConP comma [VarP x, VarP y]] (VarE z))
    | show comma == "," && x == z = l0 "fst"
    | show comma == "," && y == z = l0 "snd"

peep (AppE (LamE (VarP x:xs) y) z)
    | simple z
    = peep $ LamE xs (replaceVar x z y)

peep (AppE (AppE bind (AppE ret x)) y)
    | bind ~= ">>=" && ret ~= "return" = peep $ AppE y x

peep (InfixE (Just (AppE ret x)) bind (Just y))
    | bind ~= ">>=" && ret ~= "return" = peep $ AppE y x

peep (InfixE (Just x) op (Just y))
    | op ~= "." && x ~= "id" = y
    | op ~= "." && y ~= "id" = x
    | op ~= "&&" && y ~= "True" = x

peep (AppE append (AppE (AppE cons x) nil))
    | append ~= "++" && cons ~= ":" && nil ~= "[]"
    = peep $ AppE (l0 ":") x

peep (AppE f (LamE x (AppE (AppE cons y) nil)))
    | f ~= "concatMap" && cons ~= ":" && nil ~= "[]"
    = peep $ AppE (l0 "map") (peep $ LamE x y)

peep (CaseE (LitE x) (Match (LitP y) (NormalB z) [] : _))
    | x == y = z

-- allow easy flip to tracing mode
peep x | traceMode = trace (show x) x
peep x = x


(VarE f) ~= x = show f == x
(ConE f) ~= x = show f == x
_ ~= _ = False


simple (VarE _) = True
simple (LitE _) = True
simple _ = False
