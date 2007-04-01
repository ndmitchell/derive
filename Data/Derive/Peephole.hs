
module Data.Derive.Peephole(Peephole, peephole) where

import Language.Haskell.TH.Syntax
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


-- based on the rewrite combinator in Play
peep :: Exp -> Exp
peep (AppE f x)
    | f ~= "id" = x

peep (AppE (AppE x y) z)
    | x ~= "const" = y
    | x ~= "map" && y ~= "id" = z
    | x ~= "++" && y ~= "[]" = z
    | x ~= "++" && z ~= "[]" = y
    | x ~= "." && z ~= "id" = y

peep (AppE (AppE bind (AppE ret x)) y)
    | bind ~= ">>=" && ret ~= "return" = peep $ AppE y x

-- allow easy flip to tracing mode
peep x | False = trace (show x) x
peep x = x


(VarE f) ~= x = show f == x
_ ~= _ = False
