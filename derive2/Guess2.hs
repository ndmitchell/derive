{-# LANGUAGE PatternGuards #-}

module Guess where

import HSE
import DSL
import Data.List
import Data.Char
import Data.Generics.PlateData
import Control.Monad


ctrNames = ["CtorZero","CtorOne","CtorTwo","CtorThree"]


guess :: Res -> DSL
guess x = guessDat $ toUniverse x



guessDat :: Universe -> DSL
guessDat (UApp "InstDecl" [UList ctxt,name,typ,bod])
    | UApp "UnQual" [UApp "Ident" [UString name]] <- name
    , UList [UApp "TyApp"
        [UApp "TyCon" [UApp "UnQual" [UApp "Ident" [UString "Ctors"]]]
        ,UApp "TyVar" [UApp "Ident" [UString var]]]] <- typ
    , ctxt <- [x | UApp "ClassA" [UApp "UnQual" [UApp "Ident" [UString x]],_] <- ctxt]
    = Instance ctxt name $ guessDat bod

guessDat (UApp op xs) = App op $ map guessDat xs

guessDat (UString x) | x `elem` ctrNames = error $ "Couldn't infer constructor here: " ++ show x
                     | otherwise = String x
guessDat (UInt x) = Int x

guessDat (UList []) = List []
guessDat (UList (x:xs))
    | length (usedCtrs x) /= 1 = Append (List [guessDat x]) (guessDat (UList xs))
    | valid && clst2 == ctrNames = MapCtor $ head $ guessCtrs lst
    | valid && clst2 == reverse ctrNames = Reverse $ MapCtor $ head $ guessCtrs $ reverse lst
    | not valid = error $ "Not 4 list elements in a row"
    | otherwise = error $ "Not a valid ordering on the list elements"
        where
            (lst,rest) = splitAt 4 (x:xs)
            clst = map usedCtrs lst
            valid = all ((==) 1 . length) clst
            clst2 = map head clst


-- list must be 4 elements long, with CtorZero first
guessCtrs :: [Universe] -> [DSL]
guessCtrs xs@(x:_) =
    -- constructor names
    [CtorName | xs == map UString ctrNames] ++ 
    -- the same as each other
    [fromUni x | all (== x) xs] ++
    -- 
    concat [map (App op) $ mapM guessCtrs $ transpose ys | UApp op x1 <- [x], let ys = [x | UApp o x <- xs, o == op], length ys == 4] ++
    concat [map List $ mapM guessCtrs $ transpose lists | listsValid, length (nub listLens) == 1]
    [ | ]
    where
        lists = [x | UList x <- xs]
        listLens = map length lists
        listsValid = length lists == 4



usedCtrs :: Universe -> [String]
usedCtrs x = ctrNames `intersect` childrenBi x


{-

guessDat (UList xs) = guessList xs

gss :: Universe -> [Guess]
gss (UList xs) = concatMap f $ mapM gss xs
    where
        f xs | length (nub $ map fst is) == 1 = [GuessInt (fst $ head is) (and $ map snd is) $ \d -> List (map (($d) . stairs) xs)]
            where is = [(i,b) | GuessInt i b _ <- xs]
        f (Guess x:xs) = map (lift $ Append $ List [x]) (f xs)
        f [] = [Guess $ List []]
        
        f xs = error $ show ("here",xs, [(i,b) | GuessInt i b _ <- xs])

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


-}
