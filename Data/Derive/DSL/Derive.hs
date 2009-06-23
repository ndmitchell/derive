{-# LANGUAGE PatternGuards #-}

module Data.Derive.DSL.Derive(derive) where

import Data.Derive.DSL.HSE
import Data.Derive.DSL.DSL
import Data.Derive.DSL.Apply
import Data.List
import Data.Char
import Data.Maybe


data Guess = Guess DSL
           | GuessFld Int DSL
           | GuessCtr Int Bool DSL  -- 0 based index, does it mention CtorName
             deriving Show


ctrNames = map ctorName $ dataCtors sample


derive :: Out -> [DSL]
derive x = [simplifyDSL y | Guess y <- guess $ toOutput x]


guess :: Output -> [Guess]

guess (OApp "InstDecl" [OList ctxt,name,typ,bod])
    | OApp "UnQual" [OApp "Ident" [OString name]] <- name
    , OList [OApp "TyApp"
        [OApp "TyCon" [OApp "UnQual" [OApp "Ident" [OString nam]]]
        ,OApp "TyVar" [OApp "Ident" [OString var]]]] <- typ
    , nam == dataName sample
    , ctxt <- [x | OApp "ClassA" [OApp "UnQual" [OApp "Ident" [OString x]],_] <- ctxt]
    = [Guess $ Instance ctxt name y | Guess y <- guess bod]

guess (OList xs) = guessList xs
guess o@(OApp op xs) = gssFold o ++ gssApp o ++ map (lift (App op)) (guessList xs)
    
guess (OString x) 
    | Just i <- findIndex (`isSuffixOf` x) ctrNames = [GuessCtr i True $ String (take (length x - length (ctrNames !! i)) x) `append` CtorName]
    | "Ctors" `isSuffixOf` x = [Guess $ String (take (length x - 5) x) `append` DataName]
    | otherwise =
         [lift (\d -> append (String $ init x) (ShowInt d)) g | x /= "", isDigit (last x), g <- guess $ OInt $ read [last x]] ++
         [Guess $ String x]

guess (OInt i) =
    [GuessFld (fromInteger i) FieldIndex | i `elem` [1,2]] ++
    [GuessCtr 1 False CtorIndex | i == 1] ++
    [GuessCtr 1 False CtorArity | i == 2] ++
    [Guess $ Int i]

guess x = error $ show ("fallthrough",x)


{-
First try and figure out runs to put them in to one possible option
Then try and figure out similarities to give them the same type
-}
guessList :: [Output] -> [Guess]
guessList xs = mapMaybe sames $ map diffs $ sequence $ map guess xs
    where
        -- Given a list of guesses, try and collapse them into one coherent guess
        -- Each input Guess will guess at a List, so compose with Concat
        sames :: [Guess] -> Maybe Guess
        sames xs = do
            let (is,fs) = unzip $ map fromGuess xs
            i <- maxim is
            return $ toGuess i $ Concat $ List fs

        -- Promote each Guess to be a list
        diffs :: [Guess] -> [Guess]

        diffs (GuessCtr 0 True x0:GuessCtr 1 True x1:GuessCtr 2 True x2:xs)
            | f 0 x0 == f 0 x1 && f 2 x2 == f 2 x1 = Guess (MapCtor x1) : diffs xs
            where f i x = applyEnv x env{envInput=sample, envCtor=dataCtors sample !! i}
        
        diffs (GuessCtr 2 True x2:GuessCtr 1 True x1:GuessCtr 0 True x0:xs)
            | f 0 x0 == f 0 x1 && f 2 x2 == f 2 x1 = Guess (Reverse $ MapCtor x1) : diffs xs
            where f i x = applyEnv x env{envInput=sample, envCtor=dataCtors sample !! i}
        
        diffs (GuessFld 1 x1:GuessFld 2 x2:xs)
            | f 1 x1 == f 1 x2 = GuessCtr 1 False (MapField x2) : diffs xs
            where f i x = applyEnv x env{envInput=sample, envField=i}
        
        diffs (GuessFld 2 x2:GuessFld 1 x1:xs)
            | f 1 x1 == f 1 x2 = GuessCtr 1 False (Reverse $ MapField x2) : diffs xs
            where f i x = applyEnv x env{envInput=sample, envField=i}

        diffs (x:xs) = lift box x : diffs xs
        diffs [] = []


gssFold o@(OApp op [x,m,y]) = f True (x : follow True y) ++ f False (y : follow False x)
    where
        follow dir (OApp op2 [a,m2,b]) | op == op2 && m == m2 = a2 : follow dir b2
            where (a2,b2) = if dir then (a,b) else (b,a)
        follow dir x = [x]

        f dir xs | length xs <= 2 = []
        f dir xs = map (lift g) $ guess $ OList xs
            where g = Fold (App op $ List [h,fromOut m,t])
                  (h,t) = if dir then (Head,Tail) else (Tail,Head)

gssFold _ = []


gssApp (OApp "App" [OApp "App" [x,y],z]) = map (lift Application) $ guess $ OList $ fromApp x ++ [y,z]
    where fromApp (OApp "App" [x,y]) = fromApp x ++ [y]
          fromApp x = [x]
gssApp _ = []
    

lift :: (DSL -> DSL) -> Guess -> Guess
lift f x = toGuess a (f b)
    where (a,b) = fromGuess x


type GuessState = Maybe (Either Int (Int,Bool))

fromGuess :: Guess -> (GuessState, DSL)
fromGuess (Guess x) = (Nothing, x)
fromGuess (GuessFld i x) = (Just (Left i), x)
fromGuess (GuessCtr i b x) = (Just (Right (i,b)), x)

toGuess :: GuessState -> DSL -> Guess
toGuess Nothing = Guess
toGuess (Just (Left i)) = GuessFld i
toGuess (Just (Right (i,b))) = GuessCtr i b


-- return the maximum element, if one exists
maxim :: [GuessState] -> Maybe GuessState
maxim [] = Just Nothing
maxim [x] = Just x
maxim (Nothing:xs) = maxim xs
maxim (x:Nothing:xs) = maxim $ x:xs
maxim (x1:x2:xs) | x1 == x2 = maxim $ x1:xs
maxim (Just (Right (i1,b1)):Just (Right (i2,b2)):xs) | i1 == i2 = maxim $ Just (Right (i1,max b1 b2)) : xs
maxim _ = Nothing
