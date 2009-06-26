-- | The main TH driver module.  It is intended that this need be the
-- only module imported by user code; it takes care of all data
-- threading issues such that all one needs to do is:
--
-- @
--   data Foo = Foo ; $( derive makeEq ''Foo )
-- @
module Data.DeriveTH(derive, deriveFromDec, module Data.Derive.All) where

import Data.List
import Control.Monad

import Data.Derive.All
import Data.Derive.Internal.Derivation
import Language.Haskell.TH.All as TH hiding (Derivation,toName)
import Language.Haskell.Meta(toDec)
import Language.Haskell as H


-- | Derive an instance of some class. @derive@ only derives instances
-- for the type of the argument.
derive :: Derivation -> TH.Name -> Q [Dec]
derive d name = do
    x <- reify name
    case x of
        TyConI dec -> deriveFromDec d dec
        _ -> error $ "Data.DeriveTH.derive: Expected a data type declaration, got:\n" ++ show x

-- | Derive an instance of some class. @deriveFromDec@ only derives instances
-- for the type of the argument.
deriveFromDec :: Derivation -> Dec -> Q [Dec]
deriveFromDec d x = do
    x <- liftM normData $ expandData x
    case derivationOp d $ toFullDataDecl x of
        Left y -> runIO (putStrLn $ "Warning, couldn't derive: " ++ y) >> return []
        Right v -> return $ map toDec v


toFullDataDecl :: Dec -> FullDataDecl
toFullDataDecl x = (ModuleName "Todo", toDataDecl x)


toDataDecl :: Dec -> DataDecl
toDataDecl x = case x of
    DataD cxt n vs con ds -> f DataType cxt n vs con ds
    NewtypeD cxt n vs con ds -> f NewType cxt n vs [con] ds
    where
        f t cxt n vs con ds = DataDecl sl t (toContext cxt) (toName n) (map toTyVarBind vs) (map toQualConDecl con) []


toTyVarBind :: TH.Name -> TyVarBind
toTyVarBind = UnkindedVar . toName

toName :: TH.Name -> H.Name
toName = name . show

toQualConDecl :: Con -> QualConDecl
toQualConDecl (ForallC vs c x) = QualConDecl sl (map toTyVarBind vs) (toContext c) $ toConDecl x
toQualConDecl x = QualConDecl sl [] [] $ toConDecl x

toConDecl :: Con -> ConDecl
toConDecl (NormalC n xs) = ConDecl (toName n) (map toBangType xs)
toConDecl (RecC n xs) = RecDecl (toName n) [([toName a], toBangType (b,c)) | (a,b,c) <- xs]
toConDecl (InfixC x n y) = InfixConDecl (toBangType x) (toName n) (toBangType y)

toBangType :: StrictType -> BangType
toBangType (IsStrict, x) = BangedTy $ toType x
toBangType (NotStrict, x) = UnBangedTy $ toType x

toType :: TH.Type -> H.Type
toType (ForallT xs c t) = TyForall (Just (map toTyVarBind xs)) (toContext c) (toType t)
toType (VarT x) = TyVar $ toName x
toType (ConT x) = TyCon $ UnQual $ toName x
toType (AppT (AppT ArrowT x) y) = TyFun (toType x) (toType y)
toType (AppT ListT x) = TyList $ toType x
toType (TupleT _) = TyTuple Boxed []
toType (AppT x y) = case toType x of
    TyTuple b xs -> TyTuple b $ xs ++ [toType y]
    x -> TyApp x $ toType y

toContext :: Cxt -> Context
toContext = map toAsst

toAsst :: TH.Type -> Asst
toAsst (ConT x) = ClassA (UnQual $ toName x) []
toAsst (AppT x y) = case toAsst x of
    ClassA a b -> ClassA a (b ++ [toType y])

