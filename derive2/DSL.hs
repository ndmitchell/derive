
module DSL where

import HSE

data DSL = App String [DSL]
         | List [DSL]
         | Append DSL DSL
         | String String
         | ShowInt DSL
         
         | MapField DSL
         | MapCtor DSL
         | CtorName
         | FieldInd
         
         | Fold DSL DSL DSL
         | Head
         | Tail
         
         | Instance [String] String DSL{-[InstDecl]-}
           deriving Show

singleton x = List [x]
nil = List []


fromUni :: Universe -> DSL
fromUni (UApp x y) = App x (map fromUni y)
fromUni (UList x) = List (map fromUni x)
fromUni x = error $ show ("fromUni",x)


_1 s x1 = App s [x1]
_2 s x1 x2 = App s [x1,x2]
_3 s x1 x2 x3 = App s [x1,x2,x3]
_5 s x1 x2 x3 x4 x5 = App s [x1,x2,x3,x4,x5]

u x = fromUni $ uni x

dslEq :: DSL
dslEq = singleton $ Instance ["Eq"] "Eq" $ singleton $ _1 "InsDecl" $ _1 "FunBind" $ match `Append` dull
    where
        match = MapCtor $ _5 "Match" (u $ Symbol "==") (List [vars "x",vars "y"]) (u (Nothing :: Maybe Type)) (_1 "UnGuardedRhs" bod) (u $ BDecls [])
        vars x = _2 "PApp" (_1 "UnQual" $ _1 "Ident" CtorName) (MapField (_1 "PVar" $ _1 "Ident" $ Append (String x) (ShowInt FieldInd)))
        bod = Fold (_3 "InfixApp" Head (u $ QVarOp $ UnQual $ Symbol "&&") Tail) (u $ Con $ UnQual $ Ident "True") $ MapField pair
        pair = _3 "InfixApp" (var "x") (u $ QVarOp $ UnQual $ Symbol "==") (var "y")
        var x = _1 "Var" $ _1 "UnQual" $ _1 "Ident" $ Append (String x) (ShowInt FieldInd)

        dull = u [Match sl (Symbol "==") [PWildCard,PWildCard] Nothing (UnGuardedRhs $ Con $ UnQual $ Ident "False") (BDecls [])]

{-

uni fromUni $ uni $ InsDecl $ FunBind [m]
    where
        m = Match sl (Symbol "==") [] Nothing (UnGuardedRhs $ Lit $ Int 42) (BDecls [])
-}


{-
type Data = Decl
type Ctor = QualConDecl


dataVars :: Data -> [String]
dataVars (DataDecl _ _ _ _ x _ _) = map prettyPrint x

dataName :: Data -> String
dataName (DataDecl _ _ _ x _ _ _) = prettyPrint x


---------------------------------------------------------------------
-- DSL

-- from `elem` Data Ctor i (parameter type)
-- to `elem` HSE (result type)
data DSL from to where
    C :: to -> DSL i to
    Ap :: DSL i (a -> b) -> DSL i a -> DSL i b

    Instance :: [String] -> String -> DSL Data [InstDecl] -> DSL Data Decl



evalData :: DSL Data a -> Data -> a
evalData (C a) x = a
evalData (Ap a b) x = evalData a x $ evalData b x

evalData (Instance ctx hd body) x =
        InstDecl sl con (UnQual $ Ident hd) [typ] (evalData body x)
    where
        con = [ClassA (UnQual $ Ident c) [TyVar $ Ident v] | v <- dataVars x, c <- ctx]
        typ = foldl TyApp (TyCon $ UnQual $ Ident $ dataName x) [TyVar $ Ident v | v <- dataVars x]
        

data Sample a = Zero | One a | Two a a | Three a a


singleton :: DSL i a -> DSL i [a]
singleton x = Ap (C (:[])) x


---------------------------------------------------------------------
-- EXAMPLES


eq :: DSL Data [Decl]
eq = singleton $ Instance ["Eq"] "Eq" (C [])


test = putStr $ unlines $ map prettyPrint $ evalData eq list
-}
