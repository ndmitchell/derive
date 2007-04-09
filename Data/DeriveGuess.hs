module Data.DeriveGuess(Data2(..), guess) where

import Language.Haskell.TH.All
import Data.Generics
import Data.List
import Data.Maybe


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
guessClauses xs | null h    = error "failed to find a hyp"
                | otherwise = renderClause
    where
        renderClause = "map (\\c -> Clause " ++ list (map f info1) ++
                       " (NormalB (" ++ render info1 (head h) ++ ")) [])" ++
                       " (dataCtor dat)"
            where
                f (Terms named _) = case lookup "name" named of
                                       Nothing -> "ctp c"
                                       Just (VarE x) -> "vr " ++ show (show x)

    
        info1 = let Clause pats _ _ = head xs in info pats
    
        mid = map simplify xs
        h = filter (\h -> all (valid h) mid) $ nub $ concatMap hyps mid
        
        info pats = map f pats
            where
                f WildP = Terms [("name",VarE $ mkName "_")] []
                f (VarP x) = Terms [("all",VarE x),("name",VarE $ mkName $ getName $ show x)] []
                f (ConP name xs) = Terms [("ctor",ConE name)] [VarE x | VarP x <- xs]
        
        getName = reverse . drop 1 . dropWhile (/= '_') . reverse
        
        hyps (Clause pats (NormalB bod) []) = hypothesis (info pats) bod

        valid h (Clause pats (NormalB bod) []) = apply (info pats) h == bod


simplify :: Clause -> Clause
simplify = everywhere (mkT f)
    where
        f (InfixE (Just x) y (Just z)) = AppE (AppE y x) z
        f x = x



guessContext = list . nub . map guessPrinciple

guessPrinciple (AppT (ConT x) _) = show $ dropModule $ show x



dropModule = reverse . takeWhile (/= '.') . reverse

list x = "[" ++ concat (intersperse "," x) ++ "]"



---------------------------------------------------------------------
-- Hypothesis generation and testing


data Terms = Terms {named :: [(String,Exp)], inductive :: [Exp]}
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
hypothesis info (AppE x y) =
    [AppE xs ys | xs <- hypothesis info x, ys <- hypothesis info y] ++
    hypFold info (AppE x y)

hypothesis info x = x : [VarE $ mkName $ "#" ++ show i ++ "_" ++ name
                        | (i,Terms named _) <- zip [0..] info, (name,e) <- named, e == x]


hypFold info o@(AppE (AppE fn x) y) =
        [ TupE [VarE fold, TupE (fns : unit), TupE [VarE dir, TupE [VarE $ mkName "Map", LitE (IntegerL ind), mp]]]
        | fold <- map mkName ["Foldr","Foldl"]
        , fns <- hypothesis info fn
        , unit <- [] : map return (unwrap True o ++ unwrap False o)
        , mp <- [getMap x, getMap y]
        , ind <- getMapInd x ++ getMapInd y
        , dir <- map mkName ["Normal","Reverse"]
        ]
    where
        getMap :: Exp -> Exp
        getMap = everywhere (mkT f)
            where
                f x@(VarE _) | x `elem` concatMap inductive info = VarE $ mkName "*"
                f x = x
    
        getMapInd :: Exp -> [Integer]
        getMapInd = everything (++) ([] `mkQ` f)
            where
                f x@(VarE _) = [i | (i,xs) <- zip [0..] info, x `elem` inductive xs]
                f x = []
    
        unwrap b (AppE (AppE fn2 x) y)
            | fn2 == fn = unwrap b (if b then x else y)
        unwrap b x = hypothesis info x
hypFold _ _ = []



apply :: [Terms] -> Hypothesis -> Exp
apply info x = everywhere (mkT f) x
    where
        f (VarE x) | "#" `isPrefixOf` sx = fromJust $ lookup b $ named $ info !! read a
            where
                sx = show x
                (a,_:b) = break (== '_') (tail sx)

        f (TupE [VarE x,LitE (IntegerL i),fn])
            | show x == "Map" = ListE $ map rep $ inductive $ info !! fromInteger i
            where
                rep with = everywhere (mkT g) fn
                    where
                        g (VarE x) | show x == "*" = with
                        g x = x

        f (TupE [VarE x,ListE xs])
            | show x == "Normal"  = ListE xs
            | show x == "Reverse" = ListE (reverse xs)
        
        f (TupE [VarE x,fn,ListE ys])
            | show x == "Foldr" = g foldr foldr1
            | show x == "Foldl" = g foldl foldl1
            where
                g :: ((Exp -> Exp -> Exp) -> Exp -> [Exp] -> Exp) ->
                     ((Exp -> Exp -> Exp) -> [Exp] -> Exp) ->
                     Exp
                g app app1 = case fn of
                                 TupE [x] | null ys -> VarE $ mkName "?"
                                 TupE [x] -> g2 app1 x
                                 TupE [x,y] -> g2 (\q -> app q y) x
            
                g2 app with = app (g3 with) ys
                
                g3 :: Exp -> Exp -> Exp -> Exp
                g3 with x y = AppE (AppE with x) y

        f x = x


render :: [Terms] -> Hypothesis -> String
render x y = error $ "DeriveGuess.render: " ++ show (x,y)


