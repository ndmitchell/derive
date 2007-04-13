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

fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c

snub x = nub $ sort x

fieldCtors x = x : [3 | x == 2]
ctorFields x = if x == 3 then 2 else x

on op get a b = op (get a) (get b)


-- imagine the following environment table:
{-
[("CtorZero",0,0,[])
,("CtorOne" ,1,1,[1])
,("CtorTwo" ,2,2,[1,2])
,("CtorTwo'",2,3,[1,2])
]
-}

data Env = None | Ctor Int | Field Int
         deriving (Show,Eq)

isField (Field _) = True; isField _ = False
isCtor  (Ctor  _) = True; isCtor  _ = False

fromField (Field i) = i
fromCtor  (Ctor  i) = i

fromEnv (Field i) = i
fromEnv (Ctor  i) = i



-- Show t and Ppr t for better error messages
-- Eq t required for hypothesis testing
class (Ppr t, Eq t, Show t) => Guess t where
    -- invariant: all answers must be correct for this example
    -- will never be given a different type of environment
    guessEnv :: t -> [(Env, Env -> t, String)]
    
    guessStr :: t -> String
    guessStr t = case [s | (None,_,s) <- guessEnv t] of
                      [] -> error $ "\n\nNo hypothesis for:\n" ++ show t ++
                                    "\n\nPretty version:\n" ++ show (ppr t)
                      (x:xs) -> x



guessPairStr :: (Guess a, Guess b) => String -> a -> b -> String
guessPairStr sjoin a b = sjoin ++ " " ++ guessStr a ++ " " ++ guessStr b


joinEnvs :: [Env] -> Maybe Env
joinEnvs xs = if length ys > 1 then Nothing else Just $ head (ys ++ [None])
    where ys = filter (/= None) $ nub xs

guessOneEnv :: Guess a => (a -> t) -> String -> a -> [(Env, Env -> t, String)]
guessOneEnv fjoin sjoin x1 =
    [ (e1, \e -> fjoin (f1 e), unwordsb [sjoin,s1])
    | (e1,f1,s1) <- guessEnv x1]

-- to join two elements either they are the same env, or one has None
guessPairEnv :: (Guess a, Guess b) => (a -> b -> t) -> String -> a -> b -> [(Env, Env -> t, String)]
guessPairEnv fjoin sjoin x1 x2 =
    [ (env, \e -> fjoin (f1 e) (f2 e), unwordsb [sjoin,s1,s2])
    | (e1,f1,s1) <- guessEnv x1
    , (e2,f2,s2) <- guessEnv x2
    , Just env <- [joinEnvs [e1,e2]]]

guessTripEnv :: (Guess a, Guess b, Guess c) => (a -> b -> c -> t) -> String -> a -> b -> c -> [(Env, Env -> t, String)]
guessTripEnv fjoin sjoin x1 x2 x3 =
    [ (env, \e -> fjoin (f1 e) (f2 e) (f3 e), unwordsb [sjoin,s1,s2,s3])
    | (e1,f1,s1) <- guessEnv x1
    , (e2,f2,s2) <- guessEnv x2
    , (e3,f3,s3) <- guessEnv x3
    , Just env <- [joinEnvs [e1,e2,e3]]]



instance Guess a => Guess [a] where
    guessEnv os = concatMap f $ mapM guessEnv os
        where
            f xs | length es <= 1 = [(head (es ++ [None]), \e -> map ($ e) gens, list strs)]
                 | otherwise = [(env,gen,str) | env <- newEnvs, (gen,str) <- nubBy ((==) `on` snd) $ g xs]
                where
                    (envs,gens,strs) = unzip3 xs
                    es = nub $ filter (/= None) envs
                    
                    ctors = snub [i | Ctor i <- envs]
                    fields = snub [i | Field i <- envs]
                    maxField = maximum fields
                    
                    newEnvs = case ctors of
                                  [] -> map Ctor $ fieldCtors maxField
                                  _ | null fields -> [None]
                                  [x] | ctorFields x == maxField -> [Ctor x]
                                  _ -> []  
                    
                    ctorEnv = head newEnvs == None
                    varName = if ctorEnv then "ctor" else "field"
                    
                    domain = if ctorEnv then [0..3] else [1..maxField]
                    getDomain (Ctor i) = take 2 [1..i]
                    getDomain None = [0..3]
                    strDomain = if ctorEnv then "ctors" else "[1..ctorArity ctor]"
                    
                    construct = if ctorEnv then Ctor else Field
                    
                    g :: Eq t => [(Env, Env -> t, String)] -> [(Env -> [t], String)]
                    g [] = [(\e -> [], "")]
                    g ((None,gn,st):xs) = [(\e -> gn e : gen e, st ++ ":" ++ str) | (gen,str) <- g xs]
                    g xs =  h id "id" xs ++ h reverse "reverse" xs

                    h :: Eq t => ([Int] -> [Int]) -> String -> [(Env, Env -> t, String)] -> [(Env -> [t], String)]
                    h fdir sdir xs
                        | map construct (fdir domain) `isPrefixOf` map fst3 xs
                        = [(\e -> map (fhyp . construct) $ fdir $ getDomain e
                           ,"(map (\\" ++ varName ++ " -> " ++ shyp ++ ") (" ++ sdir ++ " " ++ strDomain ++ ")")
                          | (fhyp,shyp) <- validHyp
                          , (gen,str) <- g rest]
                        where
                            (now,rest) = splitAt (length domain) xs

                            validHyp = filter (\hyp -> all (valid (fst hyp)) now) (map (\(a,b,c) -> (b,c)) now)
                            valid hyp (e,gen,_) = hyp e == gen e

                    h _ _ _ = []



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
    guessEnv name = if sname `elem` ctorNames then guessCtor else guessRest
        where
            sname = show name
            (pre,end) = (init sname, last sname)

            
            guessCtor = [(Ctor i, \(Ctor e) -> mkName (ctorNames !! e), "(mkName (ctorName ctor))")]
                where Just i = findIndex (== sname) ctorNames

            guessRest = guessLast ++ guessDefault
            
            guessLast | isDigit end = [(e, \e -> mkName $ pre ++ show (g e)
                                       ,"(mkName (" ++ show pre ++ " ++ show " ++ s ++ "))")
                                      | (e,g,s) <- guessNum $ read [end]]
                      | otherwise   = []

            guessDefault = [(None,const name, "(mkName " ++ show sname ++ ")")
                           | not (isDigit end) || pre `notElem` ["x","y","z"]]


guessNum :: Int -> [(Env, Env -> Int, String)]
guessNum i = [(Field i, fromField, "field") | i `elem` [1,2]] ++
             [(Ctor i, fromCtor, "(ctorTag ctor)") | i `elem` [0..3]] ++
             [(Ctor i, getArity, "(ctorArity ctor)") | i `elem` [0..2]] ++
             [(Ctor 3, getArity, "(ctorArity ctor)") | i == 2]
    where
        getArity (Ctor 3) = 2
        getArity (Ctor i) = i



instance Guess Clause where
    guessEnv (Clause pat bod whr) = guessTripEnv Clause "Clause" pat bod whr


instance Guess Pat where
    guessEnv (VarP x) = guessOneEnv VarP "VarP" x
    guessEnv (ConP x xs) = guessPairEnv ConP "ConP" x xs
    guessEnv x = error $ show ("Guess Pat",x)


instance Guess Body where
    guessEnv (NormalB x) = guessOneEnv NormalB "NormalB" x
    guessEnv x = error $ show ("Guess Body",x)


instance Guess Exp where
    guessEnv o@(AppE x y) = guessFold o ++ guessPairEnv AppE "AppE" x y
    guessEnv (VarE x) = guessOneEnv VarE "VarE" x
    guessEnv (ConE x) = guessOneEnv ConE "ConE" x

    guessEnv x = error $ show ("Guess Exp",x)



guessFold :: Exp -> [(Env, Env -> Exp, String)]
guessFold o@(AppE (AppE fn x) y) =
        {- f (with foldr1) "foldr1With" (list True o) ++ -} f (with foldl1) "foldl1With" (list False o)
    where
        with fold join [] = VarE $ mkName "?"
        with fold join xs = fold (\x y -> AppE (AppE join x) y) xs
    
        list b (AppE (AppE fn2 x) y) | fn == fn2 =
            if b then x : list b y else y : list b x
        list b x = [x]

        f ffold sfold lst
            | length lst <= 2 = []
            | otherwise = error $ show (lst, map (\(a,b,c) -> (a,c)) $ guessEnv lst)
            
            
            {- [if b a == o then error $ show (a,c) - (a,b,c) - else error $ show a ++ "\n" ++ show o ++ "\n" ++ show ( b a) |
            
                            (a,b,c) <- guessPairEnv ffold sfold fn lst] -}
                            

guessFold _ = []


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
