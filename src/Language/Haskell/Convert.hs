{-# LANGUAGE CPP, ScopedTypeVariables, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Convert(Convert, convert) where

import Language.Haskell as HS
import qualified Language.Haskell.Exts as HSE(FieldDecl(..))
import Language.Haskell.TH.Compat
import Language.Haskell.TH.Syntax as TH
import Control.Exception
import Data.Typeable
import System.IO.Unsafe
import Data.Maybe


class (Typeable a, Typeable b, Show a, Show b) => Convert a b where
    conv :: a -> b


convert :: forall a b . Convert a b => a -> b
convert a = unsafePerformIO $
        (return $! (conv a :: b)) `Control.Exception.catch` (\(e :: SomeException) -> error $ msg e)
    where
        msg e = "Could not convert " ++ show (typeOf a) ++ " to " ++
                show (typeOf (undefined :: b)) ++ "\n" ++ show a ++
                "\n" ++ show e



appT :: TH.Type -> [TH.Type] -> TH.Type
appT = foldl AppT

c mr = convert mr

instance Convert a b => Convert [a] [b] where
    conv = map c



instance Convert TH.Dec (HS.Decl ()) where
    conv x = case x of
#if __GLASGOW_HASKELL__ >= 800
        DataD cxt n vs _ con ds -> f (DataType ()) cxt n vs con ds
        NewtypeD cxt n vs _ con ds -> f (NewType ()) cxt n vs [con] ds
        where
            f :: DataOrNew () -> Cxt -> TH.Name -> [TyVarBndr] -> [Con] -> unused -> HS.Decl ()
            f t cxt n vs con _ = DataDecl () t (Just $ c cxt) (dh (c n) (c vs)) (c con) []
#else
        DataD cxt n vs con ds -> f (DataType ()) cxt n vs con ds
        NewtypeD cxt n vs con ds -> f (NewType ()) cxt n vs [con] ds
        where
            f :: DataOrNew () -> Cxt -> TH.Name -> [TyVarBndr] -> [Con] -> [TH.Name] -> HS.Decl ()
            f t cxt n vs con ds = DataDecl () t (Just $ c cxt) (dh (c n) (c vs)) (c con) []
#endif

            dh name [] = DHead () name
            dh name xs = DHApp () (dh name $ init xs) (last xs)

instance Convert TH.Cxt (HS.Context ()) where
    conv = CxTuple () . map c

instance Convert (Maybe (HS.Context ())) TH.Cxt where
    conv Nothing = []
    conv (Just (CxSingle _ x)) = [c x]
    conv (Just (CxTuple _ xs)) = map c xs
    conv (Just (CxEmpty _)) = []

instance Convert TH.Name (HS.TyVarBind ()) where
    conv = UnkindedVar () . c

instance Convert TH.Name (HS.Name ()) where
    conv x = name $ if '.' `elem` x2 then reverse $ takeWhile (/= '.') $ reverse x2 else x2
        where x2 = show x

instance Convert TH.Name (HS.QName ()) where
    conv x = if x2 == Ident () "[]" then Special () $ ListCon () else UnQual () x2
        where x2 = c x

instance Convert TH.Con (HS.QualConDecl ()) where
    conv (ForallC vs cxt x) = QualConDecl () (Just $ c vs) (Just $ c cxt) (c x)
    conv x = QualConDecl () Nothing Nothing (c x)

instance Convert TH.Con (HS.ConDecl ()) where
    conv (NormalC n xs) = ConDecl () (c n) (c xs)
    conv (RecC n xs) = RecDecl () (c n) [HSE.FieldDecl () [c x] $ c (y,z) | (x,y,z) <- xs]
    conv (InfixC x n y) = InfixConDecl () (c x) (c n) (c y)

instance Convert TH.StrictType (HS.Type ()) where
#if __GLASGOW_HASKELL__ >= 800
    conv (Bang SourceUnpack SourceStrict, x) = TyBang () (BangedTy ()) (Unpack ()) $ c x
    conv (Bang SourceUnpack _, x) = TyBang () (NoStrictAnnot ()) (Unpack ()) $ c x
    conv (Bang _ SourceStrict, x) = TyBang () (BangedTy ()) (NoUnpack ()) $ c x
    conv (Bang _ _, x) = c x
#else
    conv (IsStrict, x) = TyBang () (BangedTy ()) (NoUnpack ()) $ c x
    conv (NotStrict, x) = c x
#if __GLASGOW_HASKELL__ >= 704
    conv (Unpacked, x) = TyBang () (BangedTy ()) (Unpack ()) $ c x
#endif
#endif

instance Convert TH.Type (HS.Type ()) where
    conv (ForallT xs cxt t) = TyForall () (Just $ c xs) (Just $ c cxt) (c t)
    conv (VarT x) = TyVar () $ c x
    conv (ConT x) | ',' `elem` show x = TyTuple () Boxed []
                  | otherwise = TyCon () $ c x
    conv (AppT (AppT ArrowT x) y) = TyFun () (c x) (c y)
    conv (ArrowT) = TyCon () $ Special () $ FunCon ()
    conv (AppT ListT x) = TyList () $ c x
    conv (ListT) = TyCon () $ Special () $ ListCon ()
    conv (TupleT _) = TyTuple () Boxed []
    conv (AppT x y) = case c x of
        TyTuple _ b xs -> TyTuple () b $ xs ++ [c y]
        x -> TyApp () x $ c y

instance Convert TH.Type (HS.Asst ()) where
    conv (ConT x) = ClassA () (UnQual () $ c x) []
    conv (AppT x y) = case c x of
        ClassA _ a b -> ClassA () a (b ++ [c y])

instance Convert (HS.Decl ()) TH.Dec where
    conv (InstDecl _ _ (fromIParen -> IRule _ _ cxt (fromInstHead -> (nam,typ))) ds) =
        instanceD (c cxt) (c $ tyApp (TyCon () nam) typ) [c d | InsDecl _ d <- fromMaybe [] ds]
    conv (FunBind _ ms@(HS.Match _ nam _ _ _:_)) = FunD (c nam) (c ms)
    conv (PatBind _ p bod ds) = ValD (c p) (c bod) (c ds)
    conv (TypeSig _ [nam] typ) = SigD (c nam) (c $ foralls typ)
#if __GLASGOW_HASKELL__ >= 800
    --  ! certainly BROKEN because it ignores contexts
    conv (DataDecl _ DataType{} ctx (fromDeclHead -> (nam, typ)) cs ds) =
      DataD (c ctx) (c nam) (c typ) Nothing (c cs) [] -- (c (map fst ds))
    conv (DataDecl _ NewType{} ctx (fromDeclHead -> (nam, typ)) [con] ds) =
      NewtypeD (c ctx) (c nam) (c typ) Nothing (c con) [] -- (c (map fst ds))
#else
    conv (DataDecl _ DataType{} ctx (fromDeclHead -> (nam, typ)) cs ds) =
      DataD (c ctx) (c nam) (c typ) (c cs) []
    conv (DataDecl _ NewType{} ctx (fromDeclHead -> (nam, typ)) [con] ds) =
      NewtypeD (c ctx) (c nam) (c typ) (c con) []
#endif

instance Convert (HS.QualConDecl ()) TH.Con where
    conv (QualConDecl _ Nothing Nothing con) = c con
    conv (QualConDecl _ vs cx con) = ForallC (c $ fromMaybe [] vs) (c cx) (c con)

instance Convert (HS.ConDecl ()) TH.Con where
    conv (ConDecl _ nam typ) = NormalC (c nam) (c typ)
    conv (InfixConDecl _ l nam r) = InfixC (c l) (c nam) (c r)
    conv (RecDecl _ nam fs) = RecC (c nam) (concatMap c fs)

instance Convert (HSE.FieldDecl ()) [TH.VarStrictType] where
    conv (HSE.FieldDecl _ names ty) = [(c name, bang, t) | let (bang,t) = c ty, name <- names]

instance Convert (HS.Type ()) TH.StrictType where
#if __GLASGOW_HASKELL__ >= 800
    conv (TyBang _ BangedTy{} _ t) = (Bang NoSourceUnpackedness SourceStrict, c t)
#else
    conv (TyBang _ BangedTy{} _ t) = (IsStrict, c t)
#if __GLASGOW_HASKELL__ >= 704
    conv (TyBang _ _ Unpack{} t) = (Unpacked, c t)
#else
    conv (TyBang _ _ Unpack{} t) = (IsStrict, c t)
#endif
#endif
#if __GLASGOW_HASKELL__ >= 800
    conv t = (Bang NoSourceUnpackedness NoSourceStrictness, c t)
#else
    conv t = (NotStrict, c t)
#endif

instance Convert ([HS.Name ()],HS.Type ()) [TH.VarStrictType] where
    conv (names,bt) = [(c name,s,t) | name <- names]
     where (s,t) = c bt

instance Convert (HS.Asst ()) TH.Type where
    conv (InfixA _ x y z) = c $ ClassA () y [x,z]
    conv (ClassA _ x y) = appT (ConT $ c x) (c y)

instance Convert (HS.Type ()) TH.Type where
    conv (TyCon _ (Special _ ListCon{})) = ListT
    conv (TyCon _ (Special _ UnitCon{})) = TupleT 0
    conv (TyParen _ x) = c x
    conv (TyForall _ x y z) = ForallT (c $ fromMaybe [] x) (c y) (c z)
    conv (TyVar _ x) = VarT $ c x
    conv (TyCon _ x) = if x ~= "[]" then error "here" else ConT $ c x
    conv (TyFun _ x y) = AppT (AppT ArrowT (c x)) (c y)
    conv (TyList _ x) = AppT ListT (c x)
    conv (TyTuple _ _ x) = appT (TupleT (length x)) (c x)
    conv (TyApp _ x y) = AppT (c x) (c y)

instance Convert (HS.Name ()) TH.Name where
    conv = mkName . filter (`notElem` "()") . prettyPrint

instance Convert (HS.Match ()) TH.Clause where
    conv (HS.Match _ _ ps bod ds) = Clause (c ps) (c bod) (c ds)

instance Convert (HS.Rhs ()) TH.Body where
    conv (UnGuardedRhs _ x) = NormalB (c x)
    conv (GuardedRhss _ x) = GuardedB (c x)

instance Convert (HS.Exp ()) TH.Exp where
    conv (Con _ (Special _ UnitCon{})) = TupE []
    conv (Var _ x) = VarE (c x)
    conv (Con _ x) = ConE (c x)
    conv (Lit _ x) = LitE (c x)
    conv (App _ x y) = AppE (c x) (c y)
    conv (Paren _ x) = c x
    conv (InfixApp _ x y z) = InfixE (Just $ c x) (c y) (Just $ c z)
    conv (LeftSection _ x y) = InfixE (Just $ c x) (c y) Nothing
    conv (RightSection _ y z) = InfixE Nothing (c y) (Just $ c z)
    conv (Lambda _ x y) = LamE (c x) (c y)
    conv (Tuple _ _ x) = TupE (c x)
    conv (If _ x y z) = CondE (c x) (c y) (c z)
    conv (Let _ x y) = LetE (c x) (c y)
    conv (Case _ x y) = CaseE (c x) (c y)
    conv (Do _ x) = DoE (c x)
    conv (EnumFrom _ x) = ArithSeqE $ FromR (c x)
    conv (EnumFromTo _ x y) = ArithSeqE $ FromToR (c x) (c y)
    conv (EnumFromThen _ x y) = ArithSeqE $ FromThenR (c x) (c y)
    conv (EnumFromThenTo _ x y z) = ArithSeqE $ FromThenToR (c x) (c y) (c z)
    conv (List _ x) = ListE (c x)
    conv (ExpTypeSig _ x y) = SigE (c x) (c y)
    conv (RecConstr _ x y) = RecConE (c x) (c y)
    conv (RecUpdate _ x y) = RecUpdE (c x) (c y)
    -- Work around bug 3395, convert to do notation instead
    conv (ListComp _ x y) = CompE $ c $ y ++ [QualStmt () $ Qualifier () x]

instance Convert (HS.GuardedRhs ()) (TH.Guard, TH.Exp) where
    conv (GuardedRhs _ g x) = (conv g, conv x)

instance Convert [HS.Stmt ()] TH.Guard where
    conv xs = PatG $ map conv xs

instance Convert (HS.Binds ()) [TH.Dec] where
    conv (BDecls _ x) = c x

instance Convert (Maybe (HS.Binds ())) [TH.Dec] where
    conv Nothing = []
    conv (Just x) = c x

instance Convert (HS.Pat ()) TH.Pat where
    conv (PParen _ x) = c x
    conv (PLit _ Signless{} x) = LitP (c x)
    conv (PTuple _ _ x) = TupP (c x)
    conv (PApp _ x y) = ConP (c x) (c y)
    conv (PVar _ x) = VarP (c x)
    conv (PInfixApp _ x y z) = InfixP (c x) (c y) (c z)
    conv (PIrrPat _ x) = TildeP (c x)
    conv (PAsPat _ x y) = AsP (c x) (c y)
    conv (PWildCard{}) = WildP
    conv (PRec _ x y) = RecP (c x) (c y)
    conv (PList _ x) = ListP (c x)
    conv (PatTypeSig _ x y) = SigP (c x) (c y)

instance Convert (HS.Literal ()) TH.Lit where
    conv (Char _ x _) = CharL x
    conv (String _ x _) = StringL x
    conv (Int _ x _) = IntegerL x
    conv (Frac _ x _) = RationalL x
    conv (PrimInt _ x _) = IntPrimL x
    conv (PrimWord _ x _) = WordPrimL x
    conv (PrimFloat _ x _) = FloatPrimL x
    conv (PrimDouble _ x _) = DoublePrimL x

instance Convert (HS.QName ()) TH.Name where
    conv (UnQual _ x) = c x
    conv (Qual _ m x) = c (Ident () $ prettyPrint m ++ "." ++ prettyPrint x)
    conv (Special _ (TupleCon _ Boxed i)) = Name (mkOccName $ "(" ++ replicate (i-1) ',' ++ ")") NameS

instance Convert (HS.PatField ()) TH.FieldPat where
    conv (PFieldPat _ name pat) = (c name, c pat)
    conv (PFieldPun _ name) = (c name, c $ PVar () $ Ident () $ prettyPrint name)
    conv (PFieldWildcard _) = error "Can't convert PFieldWildcard"

instance Convert (HS.QOp ()) TH.Exp where
    conv (QVarOp _ x) = c $ Var () x
    conv (QConOp _ x) = c $ Con () x

instance Convert (HS.Alt ()) TH.Match where
    conv (Alt _ x y z) = TH.Match (c x) (c y) (c z)

instance Convert (HS.Stmt ()) TH.Stmt where
    conv (Generator _ x y) = BindS (c x) (c y)
    conv (LetStmt _ x) = LetS (c x)
    conv (Qualifier _ x) = NoBindS (c x)

instance Convert (HS.QualStmt ()) TH.Stmt where
    conv (QualStmt _ x) = c x

instance Convert (HS.FieldUpdate ()) TH.FieldExp where
    conv (FieldUpdate _ x y) = (c x, c y)

instance Convert (HS.TyVarBind ()) TH.Name where
    conv (UnkindedVar _ x) = c x

#if __GLASGOW_HASKELL__ >= 612
instance Convert TH.TyVarBndr (HS.TyVarBind ()) where
    conv (PlainTV x) = UnkindedVar () $ c x
    conv (KindedTV x y) = KindedVar () (c x) $ c y

#if __GLASGOW_HASKELL__ < 706
instance Convert (TH.Kind ()) HS.Kind where
    conv StarK = KindStar
    conv (ArrowK x y) = KindFn (c x) $ c y
#else
instance Convert TH.Kind (HS.Kind ()) where
    conv StarT = KindStar ()
    conv (AppT (AppT ArrowT x) y) = KindFn () (c x) (c y)
#endif

#if __GLASGOW_HASKELL__ < 709
instance Convert TH.Pred (HS.Asst ()) where
    conv (ClassP x y) = ClassA () (UnQual () $ c x) $ c y
    conv (TH.EqualP x y) = HS.EqualP () (c x) $ c y

instance Convert (HS.Asst ()) TH.Pred where
    conv (ClassA _ x y) = ClassP (c x) $ c y
    conv (HS.EqualP _ x y) = TH.EqualP (c x) $ c y
#endif

instance Convert (HS.TyVarBind ()) TH.TyVarBndr where
    conv (UnkindedVar _ x) = PlainTV $ c x
    conv (KindedVar _ x y) = KindedTV (c x) $ c y

#if __GLASGOW_HASKELL__ < 706
instance Convert (HS.Kind ()) TH.Kind where
    conv (KindStar _) = StarK
    conv (KindFn _ x y) = ArrowK (c x) $ c y
#else
instance Convert (HS.Kind ()) TH.Kind where
    conv KindStar{} = StarT
    conv (KindFn _ x y) = AppT (AppT ArrowT (c x)) (c y)
#endif
#endif
