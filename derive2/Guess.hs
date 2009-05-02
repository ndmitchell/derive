{-# LANGUAGE PatternGuards #-}

module Guess where

import HSE
import DSL
import Data.List
import Data.Char


data Guess = Guess DSL
           | GuessInt Int Bool (DSL -> DSL)

instance Show Guess where
    show (Guess x) = show x
    show (GuessInt i b f) = "(" ++ show i ++ " " ++ show b ++ " -> " ++ show (f $ String "?") ++ ")"


guess :: Res -> [DSL]
guess x = [y | Guess y <- gss (toUniverse x)]


gss :: Universe -> [Guess]
gss (UList xs) = concatMap f $ mapM gss xs
    where
        f xs | length (nub $ map fst is) == 1 = [GuessInt (fst $ head is) (and $ map snd is) $ \d -> List (map (($d) . stairs) xs)]
            where is = [(i,b) | GuessInt i b _ <- xs]
        f (Guess x:xs) = map (lift $ Append $ List [x]) (f xs)
        f [] = [Guess $ List []]
        
        f xs = error $ show ("here",xs, [(i,b) | GuessInt i b _ <- xs])

gss (UApp "InstDecl" [UList ctxt,name,typ,bod])
    | UApp "UnQual" [UApp "Ident" [UString name]] <- name
    , UList [UApp "TyApp"
        [UApp "TyCon" [UApp "UnQual" [UApp "Ident" [UString "Ctors"]]]
        ,UApp "TyVar" [UApp "Ident" [UString var]]]] <- typ
    , ctxt <- [x | UApp "ClassA" [UApp "UnQual" [UApp "Ident" [UString x]],_] <- ctxt]
    = [Guess $ Instance ctxt name y | Guess y <- gss bod]

gss (UApp op xs) = concatMap f $ mapM gss xs
    where
        f xs | null is = [Guess $ App op [x | Guess x <- xs]]
             | length (nub $ map fst is) == 1 = [GuessInt (fst $ head is) (and $ map snd is) $ \d -> App op (map (($d) . stairs) xs)]
             | otherwise = []
            where is = [(i,b) | GuessInt i b _ <- xs]

gss (UString x) 
    | Just i <- findIndex (==x) ctrNames = [GuessInt i True (const CtorName)]
    | otherwise = [Guess $ String x] ++
        [GuessInt (read [last x]) False $ \d -> Append (String $ init x) (ShowInt d) | x /= "", isDigit (last x)]

gss x = error $ show ("fallthrough",x)

gss x = [Guess $ fromUni x]



lift :: (DSL -> DSL) -> Guess -> Guess
lift f (Guess x) = Guess (f x)
lift f (GuessInt i b g) = GuessInt i b (f . g)

stairs :: Guess -> (DSL -> DSL)
stairs (Guess x) = const x
stairs (GuessInt _ _ f) = f


ctrNames = ["CtorZero","CtorOne","CtorTwo","CtorThree"]
