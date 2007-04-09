module Data.DeriveGuess(Data2(..), guess) where

import Language.Haskell.TH.All
import Data.List


data Data2 a b = Ctor0
               | Ctor1 a
               | Ctor2 a b


guess :: Q [Dec] -> IO ()
guess x = runQ x >>= putStr . unlines . map guessDec


guessDec :: Dec -> String
guessDec (InstanceD ctx typ inner) =
    "instance_context " ++ guessContext ctx ++ " " ++
    guessPrinciple typ ++ " " ++ list (map guessDec inner)

guessDec (FunD name claus) = "FunD " ++ show (show name) ++ " " ++ guessClauses claus

guessDec x = error $ show x


guessClauses :: [Clause] -> String
guessClauses xs | null mid  = error "output raw"
                | null h    = error "failed to find a hyp"
                | otherwise = render (head h)
    where
        h = filter valid $ concatMap hyps mid
    
        (start,rest) = span isFree xs
        (mid,stop) = break isFree rest

        isFree _ = False
        
        
        hyps (Clause pats (NormalB bod) []) = hypothesis (map f pats) bod
            where
                f WildP = Terms [] []
                f (VarP x) = Terms [VarE x] []
                f (ConP name xs) = Terms [ConE name] [VarE x | VarP x <- xs]

        valid _ = error "guessClauses.valid"


guessContext = list . nub . map guessPrinciple

guessPrinciple (AppT (ConT x) _) = show $ dropModule $ show x



dropModule = reverse . takeWhile (/= '.') . reverse

list x = "[" ++ concat (intersperse "," x) ++ "]"



---------------------------------------------------------------------
-- Hypothesis generation and testing


data Terms = Terms {named :: [Exp], inductive :: [Exp]}
             deriving Show


type Hypothesis = Exp

-- special meanings within Hypothesis:
-- VarE #i_n, the n'th field of info i
-- TupE [#fold[rn][10] x1 x2]
--   r/n is reverse or normal
--   1/0 is if there is a default or not
--   x1 is the terminal element, if using 0
--   x2 is the combiner (app2 it each time)


hypothesis :: [Terms] -> Exp -> [Hypothesis]
hypothesis info x = error $ "DeriveGuess.hypothsis: " ++ show (info,x)


apply :: [Terms] -> Hypothesis -> Exp
apply x y = error $ "DeriveGuess.apply: " ++ show (x,y)


render :: Hypothesis -> String
render x = error $ "DeriveGuess.render: " ++ show x


