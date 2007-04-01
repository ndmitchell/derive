
module Data.Derive.Peephole(Peephole, peephole) where

import Language.Haskell.TH.Syntax
import Data.Derive
import Debug.Trace


class Peephole a where
    peephole :: a -> a


instance Peephole a => Peephole [a] where
    peephole = map peephole


instance (Peephole a, Peephole b) => Peephole (a,b) where
    peephole (a,b) = (peephole a, peephole b)


instance Peephole Dec where
    peephole (FunD x y) = FunD x (peephole y)
    peephole (InstanceD x y z) = InstanceD x y (peephole z)
    peephole x = x


instance Peephole Clause where
    peephole (Clause x y z) = Clause x (peephole y) (peephole z)


instance Peephole Body where
    peephole (GuardedB x) = GuardedB (peephole x)
    peephole (NormalB x) = NormalB (peephole x)


instance Peephole Guard where
    peephole (NormalG x) = NormalG (peephole x)
    peephole (PatG x) = PatG (peephole x)


instance Peephole Stmt where
    peephole (BindS x y) = BindS x (peephole y)
    peephole (LetS x) = LetS (peephole x)
    peephole (NoBindS x) = NoBindS (peephole x)
    peephole (ParS x) = ParS (peephole x)


instance Peephole Match where
    peephole (Match x y z) = Match x (peephole y) (peephole z)


instance Peephole Exp where
    peephole x = case x of
        AppE x y -> f (AppE (p x) (p y))
        LamE x y -> f (LamE x (p y))
        LetE x y -> f (LetE (p x) (p y))
        CaseE x y -> f (CaseE (p x) (p y))
        x -> f x
        where
            p x = peephole x
            f x = peep x



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
    = peephole $ LamE xs (replaceVar x z y)

peep (AppE (AppE bind (AppE ret x)) y)
    | bind ~= ">>=" && ret ~= "return" = peep $ AppE y x

peep (AppE append (AppE (AppE cons x) nil))
    | append ~= "++" && cons ~= ":" && nil ~= "[]"
    = peep $ AppE (l0 ":") x

peep (AppE f (LamE x (AppE (AppE cons y) nil)))
    | f ~= "concatMap" && cons ~= ":" && nil ~= "[]"
    = peep $ AppE (l0 "map") (peep $ LamE x y)

peep (CaseE (LitE x) (Match (LitP y) (NormalB z) [] : _))
    | x == y = z

-- allow easy flip to tracing mode
peep x | False = trace (show x) x
peep x = x


(VarE f) ~= x = show f == x
(ConE f) ~= x = show f == x
_ ~= _ = False


simple (VarE _) = True
simple (LitE _) = True
simple _ = False
