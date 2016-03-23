{-# LANGUAGE DeriveDataTypeable #-}

module Data.Derive.DSL.DSL where

import Data.Derive.DSL.HSE
import Data.List
import Data.Data
import Data.Generics.Uniplate.DataOnly

data DSL = App String DSL{-List-}
         | Concat DSL
         | Reverse DSL
         | String String
         | ShowInt DSL
         | Int Integer
         | List [DSL]
         
         | MapField DSL
         | MapCtor DSL
         | DataName
         | CtorName
         | CtorIndex
         | CtorArity
         | FieldIndex
         
         | Fold DSL DSL
         | Head
         | Tail
         
         | Instance [String] String DSL{-[InstDecl]-}
         | Application DSL{-List-}
           deriving (Data,Typeable,Show)

box x = List [x]
nil = List []
append x y = Concat $ List [x,y]


fromOut :: Output -> DSL
fromOut (OApp x y) = App x (List $ map fromOut y)
fromOut (OList x) = List (map fromOut x)
fromOut (OString x) = String x
fromOut x = error $ show ("fromOut",x)


{-
_1 s x1 = App s $ List [x1]
_2 s x1 x2 = App s $ List [x1,x2]
_3 s x1 x2 x3 = App s $ List [x1,x2,x3]
_5 s x1 x2 x3 x4 x5 = App s $ List [x1,x2,x3,x4,x5]

o x = fromOut $ out x

dslEq :: DSL
dslEq = box $ Instance ["Eq"] "Eq" $ box $ _1 "InsDecl" $ _1 "FunBind" $ match `append` dull
    where
        match = MapCtor $ _5 "Match" (o $ Symbol "==") (List [vars "x",vars "y"]) (o (Nothing :: Maybe Type)) (_1 "UnGuardedRhs" bod) (o $ BDecls [])
        vars x = _2 "PApp" (_1 "UnQual" $ _1 "Ident" CtorName) (MapField (_1 "PVar" $ _1 "Ident" $ append (String x) (ShowInt FieldIndex)))
        bod = Fold (_3 "InfixApp" Head (o $ QVarOp $ UnQual $ Symbol "&&") Tail) $ MapField pair `append` o [Con $ UnQual $ Ident "True"]
        pair = _3 "InfixApp" (var "x") (o $ QVarOp $ UnQual $ Symbol "==") (var "y")
        var x = _1 "Var" $ _1 "UnQual" $ _1 "Ident" $ append (String x) (ShowInt FieldIndex)

        dull = o [Match sl (Symbol "==") [PWildCard,PWildCard] Nothing (UnGuardedRhs $ Con $ UnQual $ Ident "False") (BDecls [])]
-}


simplifyDSL :: DSL -> DSL
simplifyDSL = transform f
    where
        f (Concat (List xs)) = case g xs of
            [x] -> x
            [] -> List []
            xs -> Concat $ List xs
        f x = x

        g (List x:List y:zs) = g $ List (x++y):zs
        g (List []:xs) = g xs
        g (String "":xs) = g xs
        g (x:xs) = x : g xs
        g [] = []


prettyTex :: DSL -> String
prettyTex = f id . transform g
    where
        bracket x = "(" ++ x ++ ")"
    
        f b (App x (List [])) = x
        f b (App x (List xs)) = b $ unwords $ x : map (f bracket) xs
        f b (App x y) = b $ x ++ " " ++ f bracket y
        f b (Concat x) = b $ "concat " ++ f bracket x
        f b (Reverse x) = b $ "reverse " ++ f bracket x
        f b (String x) = show x
        f b (ShowInt x) = b $ "showInt " ++ f bracket x
        f b (Int x) = show x
        f b (List []) = "nil"
        f b (List x) = b $ "list (" ++ concat (intersperse "," $ map (f id) x) ++ ")"
        f b (MapField x) = b $ "mapField " ++ f bracket x
        f b (MapCtor x) = b $ "mapCtor " ++ f bracket x
        f b DataName = "dataName"
        f b CtorName = "ctorName"
        f b CtorIndex = "ctorIndex"
        f b CtorArity = "ctorArity"
        f b FieldIndex = "fieldIndex"
        f b (Fold x y) = b $ "fold " ++ f bracket x ++ " " ++ f bracket y
        f b Head = "head"
        f b Tail = "tail"
        f b (Instance x y z) = b $ "instance_ " ++ show x ++ " " ++ show y ++ " " ++ f bracket z
        f b (Application x) = b $ "application " ++ f bracket x

        g (App x (List [y])) | x `elem` words "Ident UnGuardedRhs UnQual Lit" = y
        g x = x
