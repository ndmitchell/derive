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


checkGuess :: (Ppr t, Eq t, Show t) => t -> [(Env, Env -> t, String)] -> [(Env, Env -> t, String)]
checkGuess t xs = map f xs
    where
        f o@(env,gen,str) | t == gen env = o
                          | otherwise = error $ unlines ["checkGuess failed:"
                                                        ,"INPUT : " ++ show t
                                                        ,"OUTPUT: " ++ show (gen env)
                                                        ,"ENV   : " ++ show env
                                                        ,"HYP   : " ++ str
                                                        ]



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
                 | otherwise = [(env,gen,"("++str++")")
                               | env <- newEnvs, (gen,str) <- nubBy ((==) `on` snd) $ g xs]
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
                    varName = if ctorEnv then "(ctorInd,ctor)" else "field"
                    
                    domain = if ctorEnv then [0..3] else [1..maxField]
                    getDomain (Ctor i) = take 2 [1..i]
                    getDomain None = [0..3]
                    strDomain = if ctorEnv then "(zip [0..] (dataCtors dat))" else "[1..ctorArity ctor]"
                    
                    construct = if ctorEnv then Ctor else Field
                    
                    isNone x = x == None || (not ctorEnv && isCtor x)
                    
                    g :: Eq t => [(Env, Env -> t, String)] -> [(Env -> [t], String)]
                    g [] = [(\e -> [], "[]")]
                    g ((none,gn,st):xs) | isNone none =
                        [(\e -> gn e : gen e, "[" ++ st ++ "]++" ++ str) | (gen,str) <- g xs]
                    
                    g xs =  h id "id" xs ++ h reverse "reverse" xs

                    h :: Eq t => ([Int] -> [Int]) -> String -> [(Env, Env -> t, String)] -> [(Env -> [t], String)]
                    h fdir sdir xs
                        | map construct (fdir domain) `isPrefixOf` map fst3 xs
                        = [(\e -> map (fhyp . construct) (fdir $ getDomain e) ++ gen e
                           ,"(map (\\" ++ varName ++ " -> " ++ shyp ++ ") (" ++ sdir ++ " " ++ strDomain ++ "))++" ++ str)
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
        f (with foldl1) "foldl1With" (list True o) ++ f (with foldr1) "foldr1With" (list False o)
    where
        with fold join [] = VarE $ mkName "?"
        with fold join xs = fold (\y x -> AppE (AppE join x) y) xs
    
        list b (AppE (AppE fn2 x) y) | fn == fn2 =
            if b then x : list b y else y : list b x
        list b x = [x]

        f ffold sfold lst
            | length lst <= 2 = []
            | otherwise = guessPairEnv ffold sfold fn lst
                            

guessFold _ = []

