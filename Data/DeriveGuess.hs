{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Data.DeriveGuess(DataName(..), guess) where

import Language.Haskell.TH.All
import Data.Generics
import Data.List
import Data.Char
import Data.Maybe


data DataName a = CtorZero
                | CtorOne  a
                | CtorTwo  a a
                | CtorTwo' a a

ctorNames = ["CtorZero","CtorOne","CtorTwo","CtorTwo'"]


guess :: Q [Dec] -> IO ()
guess x = runQ x >>= putStr . unlines . map (widthify . guessStr . unQ)


widthify :: String -> String
widthify xs = g 80 (f xs)
    where
        g n (x:xs) | n - length x <= 0 = "\n    " ++ g 76 ([x|x/=" "] ++ xs)
                   | otherwise = x ++ g (n - length x) xs
        g n [] = ""
        
        
        f (x:xs) | isSpace x = " " : f (dropWhile isSpace xs)
        f x = case lex x of
                 [("","")] -> []
                 [(x,y)] -> x : f y


unQ :: Dec -> Dec
unQ x = normData $ everywhere (mkT g) $ everywhere (mkT f) x
    where
        f :: Name -> Name
        f name = if match s then mkName $ dropUnder s else name
            where
                s = show name
                match = isPrefixOf "_" . dropWhile isDigit . reverse

        g :: Exp -> Exp
        g (InfixE (Just x) y (Just z)) = AppE (AppE y x) z
        g x = x



dropModule = reverse . takeWhile (/= '.') . reverse
dropUnder = reverse . drop 1 . dropWhile (/= '_') . reverse

list x = "[" ++ concat (intersperse "," x) ++ "]"

unwordsb x = "(" ++ unwords x ++ ")"


patReps :: [Pat] -> (Name -> Name)
patReps pats = \x -> fromMaybe x $ lookup x rep
    where
        rep = concatMap f pats
        
        f (ConP c xs) | isJust mn = zip [x | VarP x <- xs] newvars
            where
                mn = findIndex (== show c) ctorNames
                newvars = [mkName $ "_" ++ show n ++ "_" ++ show i | let Just n = mn, i <- [1..n]]
        f x = []



data Env = None
         | Ctor Int
         | Item Int Int
         deriving (Show,Eq)


-- Show t only for debug purposes
class Show t => Guess t where
    -- invariant: all answers must be correct for this example
    guessEnv :: t -> [(Env, Env -> t, String)]
    
    guessStr :: t -> String
    guessStr t = head [s | (None,_,s) <- guessEnv t]



guessPairStr :: (Guess a, Guess b) => String -> a -> b -> String
guessPairStr sjoin a b = sjoin ++ " " ++ guessStr a ++ " " ++ guessStr b


guessOneEnv :: Guess a => (a -> t) -> String -> a -> [(Env, Env -> t, String)]
guessOneEnv fjoin sjoin x1 =
    [ (e1, \e -> fjoin (f1 e), unwordsb [sjoin,s1])
    | (e1,f1,s1) <- guessEnv x1]

-- to join two elements either they are the same env, or one has None
guessPairEnv :: (Guess a, Guess b) => (a -> b -> t) -> String -> a -> b -> [(Env, Env -> t, String)]
guessPairEnv fjoin sjoin x1 x2 =
    [ (head es, \e -> fjoin (f1 e) (f2 e), unwordsb [sjoin,s1,s2])
    | (e1,f1,s1) <- guessEnv x1
    , (e2,f2,s2) <- guessEnv x2
    , let es = nub [e1,e2]
    , length (filter (/= None) es) <= 1]

guessTripEnv :: (Guess a, Guess b, Guess c) => (a -> b -> c -> t) -> String -> a -> b -> c -> [(Env, Env -> t, String)]
guessTripEnv fjoin sjoin x1 x2 x3 =
    [ (head es, \e -> fjoin (f1 e) (f2 e) (f3 e), unwordsb [sjoin,s1,s2,s3])
    | (e1,f1,s1) <- guessEnv x1
    , (e2,f2,s2) <- guessEnv x2
    , (e3,f3,s3) <- guessEnv x3
    , let es = nub [e1,e2,e3]
    , length (filter (/= None) es) <= 1]



instance Guess a => Guess [a] where
    guessEnv [] = [(None, const [], "[]")]
    guessEnv xs = concatMap f $ mapM guessEnv xs
        where
            f xs | length ctrs <= 1 && length itms <= 1 = [(minEnv, \e -> map ($ e) gens, list strs)]
                 | length ctrs >  1 && length itms >  1 = []
                 | otherwise = induct
                where
                    (envs,gens,strs) = unzip3 xs

                    ctrs = [i | Item i _ <- envs] ++ [i | Ctor i <- envs]
                    itms = [i | Item _ i <- envs]
                    
                    minEnv = if not $ null itms then Item (head ctrs) (head itms)
                             else if not $ null ctrs then Ctor (head ctrs)
                             else None

                    -- are we inducting over constructor? 
                    -- False = over items
                    indCtr = null itms
                    
                    eenvs = map f envs
                        where
                            f (Ctor i) | indCtr = Just i
                            f (Item _ i) = Just i
                            f _ = Nothing

                    domain = if indCtr then [0..3] else error "domain here"
                    
                    
                    
                    


                    induct = [(maxEnv,error $ show ("induct",eenvs,domain)

{-


                    inductCtrs = error $ show ("Induct ctors",envs)
                    
                    inductItms = error $ show ("Induct items",envs)


                    induct (extend,insert,text,domain) = 
                        

                    induct :: (Env -> Maybe Int          -- extend
                              ,Env -> Maybe Int -> Env   -- insert
                              ,String                    -- text
                              [Int]) ->                  -- domain
                              [(Env -> [a] 
-}                            
                    



instance Guess Dec where
    guessStr (InstanceD ctx typ inner) =
            prefix ++ list (map guessStr inner)
        where
            prefix = map toLower p ++ "' dat = " ++
                     "instance_context " ++ guessContext ctx ++ " " ++
                     show p ++ " dat "

            p = guessPrinciple typ
            guessContext = list . nub . map (show . guessPrinciple)
            guessPrinciple (AppT (ConT x) _) = dropModule $ show x

    guessStr (FunD name claus) = guessPairStr "FunD" name claus


instance Guess Name where
    guessEnv name = case show name of
        x | x `elem` ctorNames -> [(Ctor i, \(Ctor e) -> mkName (ctorNames !! i), "(ctorName)")]
            where Just i = findIndex (== x) ctorNames
    
        x -> [(None,const name, show x)]


instance Guess Clause where
    -- step 1, rename all bindings as required
    guessEnv o@(Clause pat _ _) = guessTripEnv Clause "Clause" pat2 bod2 whr2
        where
            Clause pat2 bod2 whr2 = everywhere (mkT $ patReps pat) o

instance Guess Pat where
    guessEnv (VarP x) = guessOneEnv VarP "VarP" x
    guessEnv (ConP x xs) = guessPairEnv ConP "ConP" x xs
    guessEnv x = error $ show ("Guess Pat",x)


instance Guess Body where
    guessEnv (NormalB x) = guessOneEnv NormalB "NormalB" x
    guessEnv x = error $ show ("Guess Body",x)

instance Guess Exp where
    guessEnv (AppE x y) = guessPairEnv AppE "AppE" x y
    guessEnv (VarE x) = guessOneEnv VarE "VarE" x
    guessEnv (ConE x) = guessOneEnv ConE "ConE" x

    guessEnv x = error $ show ("Guess Exp",x)


{-

    

-- when guessing clauses, if you want to do constructor inductive matching
-- you must place all clauses one after another
-- and you must mention all three clauses

-- assumption: all patterns have same number of variables
instance Guess [Clause] where
    guessStr env xs =
            list (map (guessStr env) start) ++
            if null mid then "" else " ++ " ++ deps ++ " ++ " ++
            list (map (guessStr env) stop )
        where
            (start,rest) = span (isNothing . ctr) xs
            (mid,stop) = break (isNothing . ctr) rest
        
            -- Nothing is non-ctor dependent
            -- Just i is using the Ctor i
            ctr :: Clause -> Maybe Int
            ctr c@(Clause pats _ _) 
                    | length xs > 1 = error $ "Can't guess with: " ++ show c
                    | otherwise = listToMaybe xs
                where xs = [n | ConP c _ <- pats, let sc = show c
                              , "Ctor" `isPrefixOf` sc, let n = read (drop 4 sc)]

            deps = if not $ all (match (head mid)) mid then error $ show mid
                   else "map (\\c -> Clause " ++ list pats ++
                        guessListStr [ (extendCtr env (head [c | c@(ConP{}) <- ps]), bod)
                                     | c@(Clause ps bod _) <- mid] ++
                        ") ctors"
                where
                    match (Clause ps bod w) (Clause ps2 bod2 w2)
                        = null w && null w2 && map f ps == map f ps2

                    f WildP = "WildP"
                    f (VarP x) = "VarP " ++ show (show x)
                    f (ConP{}) = "ctp c 'x'"
                    
                    pats = [f p | let Clause ps _ _ = head mid, p <- ps]


instance Guess Clause where
    guessStr env x = error $ show ("Guess Clause",x)


instance Guess Body where
    guessHyp env (NormalB x) =
        [(\e -> NormalB (gen e), "(NormalB " ++ str ++ ")")
        | (gen,str) <- guessHyp env x]

instance Guess Exp where
    guessHyp env o@(AppE x y) =
        [(\e -> AppE (gen1 e) (gen2 e)
         ,"(AppE " ++ str1 ++ " " ++ str2 ++ ")")
        |(gen1,str1) <- guessHyp env x
        ,(gen2,str2) <- guessHyp env y]
        ++ guessFoldHyp env o

    guessHyp env x =
        [ (\e -> fromJust $ lookup name (concatMap named e)
          , "(fromJust (lookup " ++ show name ++ " cs))")
        | (name,x2) <- concatMap named env, x == x2] ++ f x
        where
            f (VarE x) = [(\e -> VarE x, "(VarE (mkName " ++ show (show x) ++ "))")]
            f _ = []


guessFoldHyp :: Env -> Exp -> [Hyp Exp]
guessFoldHyp env o@(AppE (AppE fn x) y) = concat
        [
            [ (\e -> let xs = fjoin e in
                     if null xs then VarE (mkName "?") else ffold (fpair e) (fjoin e)
              , "(fold1 string here)")
            | (ffold,sfold) <- [(foldr1,"foldr1"), (foldl1,"foldl1")]]
            ++
            [ (\e -> ffold (fpair e) (funit e) (fjoin e), "(fold0 string here)")
            | (ffold,sfold) <- [(foldr,"foldr"), (foldl,"foldl")]
            , (funit,sunit) <- concatMap (guessHyp env) units]
            
        | (fpair,spair) <- pairs
        , (fjoin,sjoin) <- joins
        ]
    where
        pairs :: [(Env -> Exp -> Exp -> Exp, String)]
        pairs = [(\e a b -> AppE (AppE (ffn e) a) b, "(pairs string here)")
                | (ffn,sfn) <- guessHyp env fn]

        joins :: [(Env -> [Exp], String)]
        joins = [(map fwrap . fdir . inductive . head, "(joins string here)")
                | (fwrap,swrap) <- wraps
                , (fdir ,sdir ) <- [(id,"id"),(reverse,"reverse")]]
    
    
        -- unwraps in the given direction to find a unit
        units :: [Exp]
        units = [f True o, f False o]
            where
                f b (AppE (AppE fn2 x) y) | fn2 == fn = f b (if b then x else y)
                f b x = x
    
        -- figure out what wraps (maps) each unit
        wraps :: [(Exp -> Exp, String)]
        wraps = concatMap f units
            where
                f x | x `elem` concatMap inductive env = [(id,"x")]
                f (AppE a b) = [(\x -> AppE (fa x) (fb x), "(AppE " ++ sa ++ " " ++ sb ++ ")")
                               |(fa,sa) <- f a, (fb,sb) <- f b]
                f (VarE a) = [(\x -> VarE a, "(VarE " ++ show (show a) ++ ")")]
                f _ = []

guessFoldHyp _ _ = []

-}


{-






guessDec :: Dec -> String
guessDec (InstanceD ctx typ inner) = widthify $
    map toLower p ++ "' dat = " ++
    "instance_context " ++ guessContext ctx ++ " " ++
    show p ++ " dat " ++ list (map guessDec inner)
    where
        p = guessPrinciple typ

guessDec (FunD name claus) = "funN " ++ show (show name) ++ " " ++ guessClauses claus

guessDec x = error $ show x


guessClauses :: [Clause] -> String
guessClauses xs | null h    = error "failed to find a hyp"
                | otherwise = renderClause
    where
        renderClause = "(map (\\c -> Clause " ++ list (map f info1) ++
                       " (NormalB (" ++ render info1 (head h) ++ ")) [])" ++
                       " (dataCtors dat))"
            where
                f (Terms named _) = case lookup "name" named of
                                       Nothing -> "ctp c 'x'"
                                       Just (VarE x) -> "VarP (mkName " ++ show x ++ ")"

    
        info1 = let Clause pats _ _ = head xs in info pats
    
        mid = map simplify xs
        h = filter (\h -> all (valid h) mid) $ nub $ concatMap hyps mid
        
        info pats = map f pats
            where
                f WildP = Terms [("name",VarE $ mkName "_")] []
                f (VarP x) = Terms [("all",VarE x),("name",VarE $ mkName $ show $ getName $ show x)] []
                f (ConP name xs) = Terms [("ctor",ConE name)] [VarE x | VarP x <- xs]
        
        getName = reverse . drop 1 . dropWhile (/= '_') . reverse
        
        hyps (Clause pats (NormalB bod) []) = hypothesis (info pats) bod

        valid h (Clause pats (NormalB bod) []) = apply (info pats) h == bod


simplify :: Clause -> Clause
simplify = everywhere (mkT f)
    where
        f (InfixE (Just x) y (Just z)) = AppE (AppE y x) z
        f x = x




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
                g app app1 = case fn of
                                 TupE [x] | null ys -> VarE $ mkName "?"
                                 TupE [x] -> g2 app1 x
                                 TupE [x,y] -> g2 (\q -> app q y) x
            
                g2 app with = app (g3 with) ys
                
                g3 :: Exp -> Exp -> Exp -> Exp
                g3 with x y = AppE (AppE with x) y

        f x = x


render :: [Terms] -> Hypothesis -> String
render info x = strip $ show (everywhere (mkT f) x :: Exp)
    where
        strip ('V':'a':'r':'E':' ':'@':xs) = strip xs
        strip (x:xs) = x : strip xs
        strip [] = []
        
    
        f (VarE x) | "#" `isPrefixOf` sx =
                case b of
                    "all" -> VarE $ mkName $ "@(VarE (mkName " ++ show y ++ "))"
                        where VarE y = fromJust $ lookup "name" names
                    "ctor" -> VarE $ mkName $ "@(ConE (mkName (ctorName c)))"
                    x -> error $ "render, asked for " ++ x
            where
                names = named $ info !! read a
                sx = show x
                (a,_:b) = break (== '_') (tail sx)

        f (TupE [VarE x,LitE (IntegerL i),fn])
            | show x == "Map" = VarE $ mkName $ "@(map (\\(VarE q) -> " ++ mp ++ ") (ctv c 'x'))"
            where
                mp = show (everywhere (mkT g) fn :: Exp)
                g (VarE x) | show x == "*" = VarE $ mkName "q"
                g x = x

        f (TupE [VarE x,y])
            | show x == "Normal"  = y
            | show x == "Reverse" = VarE $ mkName $ "@(reverse " ++ show y ++ ")"
        
        f (TupE [VarE x,fn,y])
            | show x == "Foldr" = g "foldr"
            | show x == "Foldl" = g "foldl"
            where
                g fold = VarE $ mkName $ '@' : case fn of
                    TupE [x] -> fold ++ "1" ++ func x ++ show y
                    TupE [x,d] -> fold ++ func x ++ "(" ++ show d ++ ") " ++ show y
                    
                func x = " (\\a b -> AppE (AppE (" ++ show x ++ ") a) b) "

        f x = x

-}
