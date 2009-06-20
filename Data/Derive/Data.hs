{-
import Data.Data

-}

module Data.Derive.Data where

import Language.Haskell.TH.All

makeData :: Derivation
makeData = undefined -- derivation genDataInst "Data"

genDataInst :: DataDef -> [Dec]
genDataInst dat = [
      instance_context ["Data","Typeable"] "Data" dat 
      [ FunD (mkName "gfoldl") (gfoldlDefs dat),
        FunD (mkName "gunfold") (gunfoldDefs dat),
        FunD (mkName "toConstr") (toConstrDefs dat),
        FunD (mkName "dataTypeOf") (dataTypeOfDefs dat) ]
    ]
toConstrDefs :: DataDef -> [Clause]
toConstrDefs dat = map toConstrImpl (zip [(1::Integer)..] (dataCtors dat)) where
    toConstrImpl (ctorInd,ctor) = Clause [toConstrParams ctor] (toConstrDef ctorInd ctor) []
    toConstrParams ctor = AsP (mkName "ctor") (matchConstructor ctor)
    toConstrDef ind _ctor = NormalB $ 
        app (varExpr "indexConstr")
            [ AppE  (varExpr "dataTypeOf") (varExpr "ctor"), lit ind ]
dataTypeOfDefs :: DataDef -> [Clause]
dataTypeOfDefs dat = [Clause dtOfParams dtOfDef dtOfClauses] where
    dtOfParams = [WildP]
    dtOfDef = NormalB $ varExpr "ty_T"
    dtOfClauses = (mkDt (dataCtors dat)) : map mkCon (zip [(1::Integer)..] (dataCtors dat))
    mkCon (ix,ctor) = FunD (mkName $ "con_C"++show ix) [Clause [] (NormalB (mkConImpl ix ctor)) []] 
    mkConImpl _ix ctor = app (varExpr "mkConstr") 
        [varExpr "ty_T", lit (ctorName ctor), ListE (fields ctor), ConE (mkName "Prefix") ]
    fields = map lit . ctorFields
    mkDt ctors = FunD (mkName "ty_T") [Clause [] (NormalB (mkDtImpl ctors)) []]
    mkDtImpl ctors = app (varExpr "mkDataType") [lit (show$ qualifiedDataName dat) , mkConVars ctors]
    mkConVars ctors = ListE $ map (\ix -> varExpr ("con_C"++show ix)) [(1::Int)..(length ctors)]
gfoldlDefs :: DataDef -> [Clause]
gfoldlDefs dat = map gfoldlImpl (zip [(1::Integer)..] (dataCtors dat)) where
    gfoldlImpl (_ctorInd,ctor) = Clause (gfoldlParams ctor) (gfoldlDef ctor) []
    gfoldlParams ctor = [ VarP (mkName "k"), VarP (mkName "r"), matchConstructor ctor ]
    gfoldlDef ctor = NormalB $ foldr1With (varExpr "k") foldFields where
        foldFields =    map (\field -> (varExpr ("x" ++ show field))) (reverse [1..ctorArity ctor])
                     ++ [ (AppE (varExpr "r") (ConE (mkName (ctorName ctor)))) ]
gunfoldDefs :: DataDef -> [Clause]
gunfoldDefs dat = [Clause guParams (NormalB guDef) []] where
    guParams = map (VarP . mkName) ["k","z","c"]
    guDef     = CaseE (AppE (varExpr "constrIndex") (varExpr "c")) $
                  map (guCase) (zip [(1::Integer)..] (dataCtors dat))
    guCase (ix,ctor) = Match (LitP (IntegerL ix)) (NormalB$ guCaseBody ctor) []
    guCaseBody ctor = foldr (\_ e -> AppE (varExpr "k") e)
                           (AppE (varExpr "z") (ConE . mkName . ctorName $ ctor))
                           [1..ctorArity ctor]
varExpr :: String -> Exp
varExpr = VarE . mkName
matchConstructor :: CtorDef -> Pat
matchConstructor ctor = ConP (mkName (ctorName ctor)) (fields++[]) where
    fields = map (\field -> (VarP (mkName ("x" ++ show field))))
                 (id [1..ctorArity ctor])
