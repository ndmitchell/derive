{-# LANGUAGE CPP, ScopedTypeVariables, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Language.Haskell.Convert(Convert, convert) where

import Language.Haskell as HS
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



appT = foldl AppT

c mr = convert mr

instance Convert a b => Convert [a] [b] where
    conv = map c



instance Convert TH.Dec HS.Decl where
    conv x = case x of
        DataD cxt n vs con ds -> f DataType cxt n vs con ds
        NewtypeD cxt n vs con ds -> f NewType cxt n vs [con] ds
        where
            f t cxt n vs con ds = DataDecl sl t (c cxt) (c n) (c vs) (c con) []

instance Convert TH.Name HS.TyVarBind where
    conv = UnkindedVar . c

instance Convert TH.Name HS.Name where
    conv x = name $ if '.' `elem` x2 then reverse $ takeWhile (/= '.') $ reverse x2 else x2
        where x2 = show x

instance Convert TH.Name HS.QName where
    conv x = if x2 == Ident "[]" then Special ListCon else UnQual x2
        where x2 = c x

instance Convert TH.Con HS.QualConDecl where
    conv (ForallC vs cxt x) = QualConDecl sl (c vs) (c cxt) (c x)
    conv x = QualConDecl sl [] [] (c x)

instance Convert TH.Con HS.ConDecl where
    conv (NormalC n xs) = ConDecl (c n) (c xs)
    conv (RecC n xs) = RecDecl (c n) [([c x], c (y,z)) | (x,y,z) <- xs]
    conv (InfixC x n y) = InfixConDecl (c x) (c n) (c y)

instance Convert TH.StrictType HS.BangType where
    conv (IsStrict, x) = BangedTy $ c x
    conv (NotStrict, x) = UnBangedTy $ c x
#if __GLASGOW_HASKELL__ >= 704
    conv (Unpacked, x) = BangedTy $ c x
#endif

instance Convert TH.Type HS.Type where
    conv (ForallT xs cxt t) = TyForall (Just $ c xs) (c cxt) (c t)
    conv (VarT x) = TyVar $ c x
    conv (ConT x) | ',' `elem` show x = TyTuple Boxed []
                  | otherwise = TyCon $ c x
    conv (AppT (AppT ArrowT x) y) = TyFun (c x) (c y)
    conv (AppT ListT x) = TyList $ c x
    conv (TupleT _) = TyTuple Boxed []
    conv (AppT x y) = case c x of
        TyTuple b xs -> TyTuple b $ xs ++ [c y]
        x -> TyApp x $ c y

instance Convert TH.Type HS.Asst where
    conv (ConT x) = ClassA (UnQual $ c x) []
    conv (AppT x y) = case c x of
        ClassA a b -> ClassA a (b ++ [c y])



instance Convert HS.Decl TH.Dec where
    conv (InstDecl _ cxt nam typ ds) = InstanceD (c cxt) (c $ tyApp (TyCon nam) typ) [c d | InsDecl d <- ds]
    conv (FunBind ms@(HS.Match _ nam _ _ _ _:_)) = FunD (c nam) (c ms)
    conv (PatBind _ p _ bod ds) = ValD (c p) (c bod) (c ds)
    conv (TypeSig _ [nam] typ) = SigD (c nam) (c $ foralls typ)
    conv (DataDecl _ DataType ctx nam typ cs ds) =
      DataD (c ctx) (c nam) (c typ) (c cs) (c (map fst ds))
    conv (DataDecl _ NewType ctx nam typ [con] ds) =
      NewtypeD (c ctx) (c nam) (c typ) (c con) (c (map fst ds))

instance Convert HS.QualConDecl TH.Con where
    conv (QualConDecl _ [] [] con) = c con
    conv (QualConDecl _ vs cx con) = ForallC (c vs) (c cx) (c con)

instance Convert HS.ConDecl TH.Con where
    conv (ConDecl nam typ) = NormalC (c nam) (c typ)
    conv (InfixConDecl l nam r) = InfixC (c l) (c nam) (c r)
    conv (RecDecl nam fs) = RecC (c nam) (concatMap c fs)

instance Convert HS.BangType TH.StrictType where
    conv (BangedTy t) = (IsStrict,c t)
    conv (UnBangedTy t) = (NotStrict,c t)

instance Convert ([HS.Name],HS.BangType) [TH.VarStrictType] where
    conv (names,bt) = [(c name,s,t) | name <- names]
     where (s,t) = c bt

instance Convert HS.Asst TH.Type where
    conv (InfixA x y z) = c $ ClassA y [x,z]
    conv (ClassA x y) = appT (ConT $ c x) (c y)

instance Convert HS.Type TH.Type where
    conv (TyCon (Special ListCon)) = ListT
    conv (TyCon (Special UnitCon)) = TupleT 0
    conv (TyParen x) = c x
    conv (TyForall x y z) = ForallT (c $ fromMaybe [] x) (c y) (c z)
    conv (TyVar x) = VarT $ c x
    conv (TyCon x) = if x ~= "[]" then error "here" else ConT $ c x
    conv (TyFun x y) = AppT (AppT ArrowT (c x)) (c y)
    conv (TyList x) = AppT ListT (c x)
    conv (TyTuple _ x) = appT (TupleT (length x)) (c x)
    conv (TyApp x y) = AppT (c x) (c y)

instance Convert HS.Name TH.Name where
    conv = mkName . filter (`notElem` "()") . prettyPrint

instance Convert HS.Match TH.Clause where
    conv (HS.Match _ _ ps _ bod ds) = Clause (c ps) (c bod) (c ds)

instance Convert HS.Rhs TH.Body where
    conv (UnGuardedRhs x) = NormalB (c x)
    conv (GuardedRhss x) = GuardedB (c x)

instance Convert HS.Exp TH.Exp where
    conv (Con (Special UnitCon)) = TupE []
    conv (Var x) = VarE (c x)
    conv (Con x) = ConE (c x)
    conv (Lit x) = LitE (c x)
    conv (App x y) = AppE (c x) (c y)
    conv (Paren x) = c x
    conv (InfixApp x y z) = InfixE (Just $ c x) (c y) (Just $ c z)
    conv (LeftSection x y) = InfixE (Just $ c x) (c y) Nothing
    conv (RightSection y z) = InfixE Nothing (c y) (Just $ c z)
    conv (Lambda _ x y) = LamE (c x) (c y)
    conv (Tuple _ x) = TupE (c x)
    conv (If x y z) = CondE (c x) (c y) (c z)
    conv (Let x y) = LetE (c x) (c y)
    conv (Case x y) = CaseE (c x) (c y)
    conv (Do x) = DoE (c x)
    conv (EnumFrom x) = ArithSeqE $ FromR (c x)
    conv (EnumFromTo x y) = ArithSeqE $ FromToR (c x) (c y)
    conv (EnumFromThen x y) = ArithSeqE $ FromThenR (c x) (c y)
    conv (EnumFromThenTo x y z) = ArithSeqE $ FromThenToR (c x) (c y) (c z)
    conv (List x) = ListE (c x)
    conv (ExpTypeSig _ x y) = SigE (c x) (c y)
    conv (RecConstr x y) = RecConE (c x) (c y)
    conv (RecUpdate x y) = RecUpdE (c x) (c y) 
    -- Work around bug 3395, convert to do notation instead
    conv (ListComp x y) = CompE $ c $ y ++ [QualStmt $ Qualifier x]

instance Convert HS.GuardedRhs (TH.Guard, TH.Exp) where
    conv = undefined

instance Convert HS.Binds [TH.Dec] where
    conv (BDecls x) = c x

instance Convert HS.Pat TH.Pat where
    conv (PParen x) = c x
    conv (PLit x) = LitP (c x)
    conv (PTuple _ x) = TupP (c x)
    conv (PApp x y) = ConP (c x) (c y)
    conv (PVar x) = VarP (c x)
    conv (PInfixApp x y z) = InfixP (c x) (c y) (c z)
    conv (PIrrPat x) = TildeP (c x)
    conv (PAsPat x y) = AsP (c x) (c y)
    conv (PWildCard) = WildP
    conv (PRec x y) = RecP (c x) (c y)
    conv (PList x) = ListP (c x)
    conv (PatTypeSig _ x y) = SigP (c x) (c y)

instance Convert HS.Literal TH.Lit where
    conv (Char x) = CharL x
    conv (String x) = StringL x
    conv (Int x) = IntegerL x
    conv (Frac x) = RationalL x
    conv (PrimInt x) = IntPrimL x
    conv (PrimWord x) = WordPrimL x
    conv (PrimFloat x) = FloatPrimL x
    conv (PrimDouble x) = DoublePrimL x

instance Convert HS.QName TH.Name where
    conv (UnQual x) = c x
    conv (Qual m x) = c (Ident $ prettyPrint m ++ "." ++ prettyPrint x)
    conv (Special (TupleCon Boxed i)) = Name (mkOccName $ "(" ++ replicate (i-1) ',' ++ ")") NameS

instance Convert HS.PatField TH.FieldPat where
    conv = undefined

instance Convert HS.QOp TH.Exp where
    conv (QVarOp x) = c $ Var x
    conv (QConOp x) = c $ Con x

instance Convert HS.Alt TH.Match where
    conv (Alt _ x y z) = TH.Match (c x) (c y) (c z)

instance Convert HS.Stmt TH.Stmt where
    conv (Generator _ x y) = BindS (c x) (c y)
    conv (LetStmt x) = LetS (c x)
    conv (Qualifier x) = NoBindS (c x)

instance Convert HS.QualStmt TH.Stmt where
    conv (QualStmt x) = c x

instance Convert HS.FieldUpdate TH.FieldExp where
    conv (FieldUpdate x y) = (c x, c y)

instance Convert HS.TyVarBind TH.Name where
    conv (UnkindedVar x) = c x

instance Convert HS.GuardedAlts TH.Body where
    conv (UnGuardedAlt x) = NormalB (c x)
    conv (GuardedAlts x) = GuardedB (c x)

instance Convert HS.GuardedAlt (TH.Guard, TH.Exp) where
    conv (GuardedAlt _ x y) = (PatG (c x), c y)


#if __GLASGOW_HASKELL__ >= 612
instance Convert TH.TyVarBndr HS.TyVarBind where
    conv (PlainTV x) = UnkindedVar $ c x
    conv (KindedTV x y) = KindedVar (c x) $ c y

#if __GLASGOW_HASKELL__ < 706
instance Convert TH.Kind HS.Kind where
    conv StarK = KindStar
    conv (ArrowK x y) = KindFn (c x) $ c y
#else
instance Convert TH.Kind HS.Kind where
    conv StarT = KindStar
    conv (AppT (AppT ArrowT x) y) = KindFn (c x) (c y)
#endif

#if __GLASGOW_HASKELL__ < 709
instance Convert TH.Pred HS.Asst where
    conv (ClassP x y) = ClassA (UnQual $ c x) $ c y
    conv (TH.EqualP x y) = HS.EqualP (c x) $ c y

instance Convert HS.Asst TH.Pred where
    conv (ClassA x y) = ClassP (c x) $ c y
    conv (HS.EqualP x y) = TH.EqualP (c x) $ c y
#endif

instance Convert HS.TyVarBind TH.TyVarBndr where
    conv (UnkindedVar x) = PlainTV $ c x
    conv (KindedVar x y) = KindedTV (c x) $ c y

#if __GLASGOW_HASKELL__ < 706
instance Convert HS.Kind TH.Kind where
    conv KindStar = StarK
    conv (KindFn x y) = ArrowK (c x) $ c y
#else
instance Convert HS.Kind TH.Kind where
    conv KindStar = StarT
    conv (KindFn x y) = AppT (AppT ArrowT (c x)) (c y)
#endif
#endif
