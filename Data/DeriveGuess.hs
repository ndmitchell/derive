{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Data.DeriveGuess(DataName(..), tup1, guess) where

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

-- | Guess an instantiator from a sample instance.
guess :: (String, Q [Dec]) -> IO ()
guess (name,x) = runQ x >>= putStr . (++) line0. widthify . (++) line1 . guessStr . unQ
    where
        line0 = "make" ++ name ++ " = Derivation " ++ lname ++ "' \"" ++ name ++ "\"\n"
        line1 = lname ++ "' dat = "
        lname = toLower (head name) : tail name

-- | A fake constructor for the unary tuple.  Helps 'guess' to see
-- patterns in progressions of differently sized tuples.
tup1 = id

-- | Chop and mangle a String representing Haskell code so that it
-- fits in 80 columns, without regard for prettiness.
widthify :: String -> String
widthify xs = g 80 (f xs)
    where
        g n (x:xs) | n - length x <= 0 = "\n    " ++ g 76 ([x|x/=" "] ++ xs)
                   | otherwise = x ++ g (n - length x) xs
        g n [] = "\n"


        f (x:xs) | isSpace x = " " : f (dropWhile isSpace xs)
        f x = case lex x of
                 [("","")] -> []
                 -- \\ must not occur at the end of a line (CPP restrictions)
                 [("\\",y)] -> let a:b = f y in ('\\':a) : b
                 [(x,y)] -> x : f y

-- | Process a tree produced by a quasiquote, stripping name
-- uniquifiers and changing applications and tuplings into a standard
-- form.
unQ :: [Dec] -> [Dec]
unQ x = everywhere (mkT g) $ everywhere (mkT f) $ map normData x
    where
        -- | Remove_0 evil_1 ghc_2 name_3 uniquifiers_4
        f :: Name -> Name
        f name = if match s then mkName $ dropUnder s else name
            where
                s = show name
                match = isPrefixOf "_" . dropWhile isDigit . reverse

        -- | Turn infix applications into prefix, and normalise
        -- tuples.
        g :: Exp -> Exp
        g (InfixE (Just x) y (Just z)) = AppE (AppE y x) z
        g (AppE (VarE tup) x) | show tup == "tup1" = TupE [x]
        g (ConE unit) | show unit == "()" = TupE []
        g x = x


-- | Extract the module from a qualified name.
dropModule = reverse . takeWhile (/= '.') . reverse
-- | Drop the first _ and everything after it; used to trim GHC name
-- uniques.
dropUnder = reverse . drop 1 . dropWhile (/= '_') . reverse

list x = "[" ++ concat (intersperse "," x) ++ "]"

unwordsb x = "(" ++ unwords x ++ ")"

fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c

snub x = nub $ sort x

arityToCtors x = x : [3 | x == 2]
ctorToArity  x = if x == 3 then 2 else x

ctorArityEnv (Ctor i) = ctorToArity i

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
    
guessStr :: Guess t => t -> String
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


guessEnvStr :: Guess t => t -> [(Env, Env -> t, String)]
guessEnvStr t = [(None, const t, guessStr t)]


guessPairStr :: (Guess a, Guess b) => String -> a -> b -> String
guessPairStr sjoin a b = sjoin ++ " " ++ guessStr a ++ " " ++ guessStr b

guessTripStr :: (Guess a, Guess b, Guess c) => String -> a -> b -> c -> String
guessTripStr sjoin a b c = unwords [sjoin, guessStr a, guessStr b, guessStr c]


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
            -- first try and induct based on the length of the list
            f xs | all (== None) (map fst3 xs) &&
                   length xs == 2 &&
                   length vals == 1
                 = [(Ctor i, \e -> replicate (ctorArityEnv e) (head vals),
                             "(replicate (ctorArity ctor) " ++ thd3 (head xs) ++ ")")
                   | i <- [2,3]] ++
                   [(None, \e -> map ($ e) gens, list strs)]
                 where
                    (envs,gens,strs) = unzip3 xs
                    vals = nub $ zipWith ($) gens envs

        
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
                                  [] -> map Ctor $ arityToCtors maxField
                                  _ | null fields -> [None]
                                  [x] | ctorToArity x == maxField -> [Ctor x]
                                  _ -> []  
                    
                    ctorEnv = head newEnvs == None
                    varName = if ctorEnv then "(ctorInd,ctor)" else "field"
                    
                    domain = if ctorEnv then [0..3] else [1..maxField]
                    getDomain (Ctor i) = take 2 [1..i]
                    getDomain None = [0..3]
                    strDomain = if ctorEnv then "(zip [0..] (dataCtors dat))" else "[1..ctorToArity ctor]"
                    
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
    guessEnv (InstanceD ctx typ inner) =
            [(None, \e -> InstanceD ctx typ (gen e), prefix ++ str)
            |(None,gen,str) <- guessEnv inner]
        where
            prefix = "instance_context " ++ guessContext ctx ++ " " ++
                     show p ++ " dat "

            p = guessPrinciple typ
            guessContext = list . nub . map (show . guessPrinciple)
            guessPrinciple (AppT (ConT x) _) = dropModule $ show x

    guessEnv (FunD name claus) = guessPairEnv FunD "FunD" name claus
    guessEnv (ValD pat bod whr) = guessTripEnv ValD "ValD" pat bod whr
    
    guessEnv x = error $ show ("Guess Dec",x)


instance Guess Name where
    guessEnv name = if null guessCtor then guessRest else guessCtor
        where
            sname = show name
            (pre,end) = (init sname, last sname)
            
            guessCtor = [(Ctor i, \(Ctor e) -> mkName (pre ++ (ctorNames !! e))
                                ,"(mkName (" ++ show pre ++ " ++ ctorName ctor))")
                        | (i,nam) <- zip [0..] ctorNames, nam `isSuffixOf` sname
                        , let pre = take (length sname - length nam) sname]

            guessRest = guessLast ++ guessDefault
            
            guessLast | isDigit end = [(e, \e -> mkName $ pre ++ show (g e)
                                       ,"(mkName (" ++ show pre ++ " ++ show " ++ s ++ "))")
                                      | (e,g,s) <- guessNum $ read [end]]
                      | otherwise   = []

            guessDefault = [(None,const name, "(mkName " ++ show sname ++ ")")
                           | not (isDigit end) || pre `notElem` ["x","y","z"]]


guessNum :: Int -> [(Env, Env -> Int, String)]
guessNum i = [(Field i, fromField, "field") | i `elem` [1,2]] ++
             [(None, const 3, "(toInteger (length (dataCtors dat) - 1))") | i == 3] ++
             [(None, const 4, "(toInteger (length (dataCtors dat)))") | i == 4] ++
             [(Ctor i, fromCtor, "ctorInd") | i `elem` [0..3]] ++
             [(Ctor i, ctorArityEnv, "(ctorArity ctor)") | i `elem` [0..2]] ++
             [(Ctor 3, ctorArityEnv, "(ctorArity ctor)") | i == 2]



instance Guess Clause where
    guessEnv (Clause pat bod whr) = guessTripEnv Clause "Clause" pat bod whr


instance Guess Stmt where
    guessEnv (BindS x y) = guessPairEnv BindS "BindS" x y
    guessEnv (NoBindS x) = guessOneEnv NoBindS "NoBindS" x
    guessEnv x = error $ show ("Guess Stmt",x)


instance Guess Pat where
    guessEnv (VarP x) = guessOneEnv VarP "VarP" x
    guessEnv (ConP x xs) = guessPairEnv ConP "ConP" x xs
    guessEnv (WildP) = [(None, const WildP, "WildP")]
    guessEnv (TildeP x) = guessOneEnv TildeP "TildeP" x
    guessEnv (RecP x []) = guessOneEnv (flip RecP []) "(flip RecP [])" x
    guessEnv (LitP x) = guessOneEnv LitP "LitP" x
    guessEnv x = error $ show ("Guess Pat",x)


instance Guess Body where
    guessEnv (NormalB x) = guessOneEnv NormalB "NormalB" x
    guessEnv x = error $ show ("Guess Body",x)


instance Guess Exp where
    guessEnv (VarE x) = guessOneEnv VarE "VarE" x
    guessEnv (ConE x) = guessOneEnv ConE "ConE" x
    guessEnv (LitE x) = guessOneEnv LitE "LitE" x
    guessEnv (ListE x) = guessOneEnv ListE "ListE" x
    guessEnv (LamE x y) = guessPairEnv LamE "LamE" x y
    guessEnv (CompE x) = guessOneEnv CompE "CompE" x
    guessEnv (CaseE x y) = guessPairEnv CaseE "CaseE" x y
    guessEnv (TupE x) = guessOneEnv TupE "TupE" x
    guessEnv (RecConE x []) = guessOneEnv (flip RecConE []) "(flip RecConE [])" x
    guessEnv (CondE x y z) = guessTripEnv CondE "CondE" x y z
    guessEnv (DoE x) = guessOneEnv DoE "DoE" x

    guessEnv o@(AppE x y) = guessApply o ++ guessFold o ++ guessPairEnv AppE "AppE" x y
    
    guessEnv x = error $ show ("Guess Exp",x)


instance Guess Match where
    guessEnv (Match a b c) = guessTripEnv Match "Match" a b c


instance Guess Lit where
    guessEnv o@(IntegerL i) =
        [ (env, \e -> IntegerL $ toInteger $ gen e, "(IntegerL " ++ str ++ ")")
        | (env,gen,str) <- guessNum $ fromInteger i] ++
        [(None,const $ IntegerL i,"(IntegerL " ++ show i ++ ")")]
    
    guessEnv o@(StringL s) | s == "DataName" = [(None, const o, "(StringL (dataName dat))")]
                           | otherwise = [(None, const o, "(StringL " ++ show s ++ ")")]
    
    guessEnv x = error $ show ("Guess Lit",x)


-- for when an expression is just an application
guessApply :: Exp -> [(Env, Env -> Exp, String)]
guessApply o | length args <= 1 = []
             | otherwise = guessPairEnv applyWith "applyWith" fn args
    where
        (fn,args) = list o
    
        list (AppE x y) = let (fn,args) = list x in (fn, args ++ [y])
        list x = (x, [])


-- for when an expression comes from folding
guessFold :: Exp -> [(Env, Env -> Exp, String)]
guessFold o@(AppE (AppE fn x) y) =
        f (with foldl1With) "foldl1With" (list True  o) ++
        f (with foldr1With) "foldr1With" (list False o)
    where
        with fold join [] = VarE $ mkName "?"
        with fold join xs = fold join xs
    
        list b (AppE (AppE fn2 x) y) | fn == fn2 =
            if b then x : list b y else y : list b x
        list b x = [x]

        f ffold sfold lst
            | length lst <= 2 = []
            | otherwise = guessPairEnv ffold sfold fn lst

guessFold _ = []

