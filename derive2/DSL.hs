{-# LANGUAGE DeriveDataTypeable #-}

module DSL where

import HSE
import Data.Data

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
         | CtorInd
         | CtorArity
         | FieldInd
         
         | Fold DSL DSL
         | Head
         | Tail
         
         | Instance [String] String DSL{-[InstDecl]-}
         | Application DSL{-List-}
           deriving (Data,Typeable,Show)

box x = List [x]
nil = List []
append x y = Concat $ List [x,y]


fromUni :: Universe -> DSL
fromUni (UApp x y) = App x (List $ map fromUni y)
fromUni (UList x) = List (map fromUni x)
fromUni (UString x) = String x
fromUni x = error $ show ("fromUni",x)


_1 s x1 = App s $ List [x1]
_2 s x1 x2 = App s $ List [x1,x2]
_3 s x1 x2 x3 = App s $ List [x1,x2,x3]
_5 s x1 x2 x3 x4 x5 = App s $ List [x1,x2,x3,x4,x5]

u x = fromUni $ uni x

dslEq :: DSL
dslEq = box $ Instance ["Eq"] "Eq" $ box $ _1 "InsDecl" $ _1 "FunBind" $ match `append` dull
    where
        match = MapCtor $ _5 "Match" (u $ Symbol "==") (List [vars "x",vars "y"]) (u (Nothing :: Maybe Type)) (_1 "UnGuardedRhs" bod) (u $ BDecls [])
        vars x = _2 "PApp" (_1 "UnQual" $ _1 "Ident" CtorName) (MapField (_1 "PVar" $ _1 "Ident" $ append (String x) (ShowInt FieldInd)))
        bod = Fold (_3 "InfixApp" Head (u $ QVarOp $ UnQual $ Symbol "&&") Tail) $ MapField pair `append` u [Con $ UnQual $ Ident "True"]
        pair = _3 "InfixApp" (var "x") (u $ QVarOp $ UnQual $ Symbol "==") (var "y")
        var x = _1 "Var" $ _1 "UnQual" $ _1 "Ident" $ append (String x) (ShowInt FieldInd)

        dull = u [Match sl (Symbol "==") [PWildCard,PWildCard] Nothing (UnGuardedRhs $ Con $ UnQual $ Ident "False") (BDecls [])]
