
module Data.Derive.Eq(derive) where

import Data.Derive
import Data.List

derive :: DataDef -> [String]
derive dat@(DataDef name arity ctors) =
        instanceHead "Eq" dat : map (("    " ++) . f) ctors ++ ["    _ == _ = False"]
    where
        f (CtorDef name arity _)
            | arity == 0 = name ++ " == " ++ name ++ " = True"
            | otherwise = lhs 'a' ++ " == " ++ lhs 'b' ++ " = " ++ concat (intersperse " && " $ map rhs [1..arity])
            where
                lhs i = "(" ++ name ++ concat [' ':i:show j | j <- [1..arity]] ++ ")"
                rhs i = ('a':show i) ++ " == " ++ ('b':show i)
