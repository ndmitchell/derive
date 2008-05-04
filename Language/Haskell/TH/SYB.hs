{-# OPTIONS_GHC -cpp -fno-warn-missing-methods -fno-warn-deprecations -fno-warn-unused-imports #-}

module Language.Haskell.TH.SYB where

import Data.Generics hiding (Fixity)
import Language.Haskell.TH.Syntax
import Data.PackedString

#ifdef TH_SYB
instance Data PackedString where gfoldl k z x = z x


typename_Name = mkTyCon "Name"
instance Typeable Name
    where typeOf _ = mkTyConApp typename_Name ([])



instance Data Name
    where gfoldl k z (Name x1 x2) = k (k (z Name) x1) x2



typename_NameFlavour = mkTyCon "NameFlavour"
instance Typeable NameFlavour
    where typeOf _ = mkTyConApp typename_NameFlavour ([])



instance Data NameFlavour
    where gfoldl k z x = z x



typename_NameSpace = mkTyCon "NameSpace"
instance Typeable NameSpace
    where typeOf _ = mkTyConApp typename_NameSpace ([])



instance Data NameSpace
    where gfoldl k z (VarName) = z VarName
          gfoldl k z (DataName) = z DataName
          gfoldl k z (TcClsName) = z TcClsName



typename_Info = mkTyCon "Info"
instance Typeable Info
    where typeOf _ = mkTyConApp typename_Info ([])



instance Data Info
    where gfoldl k z (ClassI x1) = k (z ClassI) x1
          gfoldl k z (ClassOpI x1
                               x2
                               x3
                               x4) = k (k (k (k (z ClassOpI) x1) x2) x3) x4
          gfoldl k z (TyConI x1) = k (z TyConI) x1
          gfoldl k z (PrimTyConI x1
                                 x2
                                 x3) = k (k (k (z PrimTyConI) x1) x2) x3
          gfoldl k z (DataConI x1
                               x2
                               x3
                               x4) = k (k (k (k (z DataConI) x1) x2) x3) x4
          gfoldl k z (VarI x1 x2 x3 x4) = k (k (k (k (z VarI) x1) x2) x3) x4
          gfoldl k z (TyVarI x1 x2) = k (k (z TyVarI) x1) x2



typename_Fixity = mkTyCon "Fixity"
instance Typeable Fixity
    where typeOf _ = mkTyConApp typename_Fixity ([])



instance Data Fixity
    where gfoldl k z (Fixity x1 x2) = k (k (z Fixity) x1) x2



typename_FixityDirection = mkTyCon "FixityDirection"
instance Typeable FixityDirection
    where typeOf _ = mkTyConApp typename_FixityDirection ([])



instance Data FixityDirection
    where gfoldl k z (InfixL) = z InfixL
          gfoldl k z (InfixR) = z InfixR
          gfoldl k z (InfixN) = z InfixN



typename_Lit = mkTyCon "Lit"
instance Typeable Lit
    where typeOf _ = mkTyConApp typename_Lit ([])



instance Data Lit
    where gfoldl k z (CharL x1) = k (z CharL) x1
          gfoldl k z (StringL x1) = k (z StringL) x1
          gfoldl k z (IntegerL x1) = k (z IntegerL) x1
          gfoldl k z (RationalL x1) = k (z RationalL) x1
          gfoldl k z (IntPrimL x1) = k (z IntPrimL) x1
          gfoldl k z (FloatPrimL x1) = k (z FloatPrimL) x1
          gfoldl k z (DoublePrimL x1) = k (z DoublePrimL) x1



typename_Pat = mkTyCon "Pat"
instance Typeable Pat
    where typeOf _ = mkTyConApp typename_Pat ([])



instance Data Pat
    where gfoldl k z (LitP x1) = k (z LitP) x1
          gfoldl k z (VarP x1) = k (z VarP) x1
          gfoldl k z (TupP x1) = k (z TupP) x1
          gfoldl k z (ConP x1 x2) = k (k (z ConP) x1) x2
          gfoldl k z (InfixP x1 x2 x3) = k (k (k (z InfixP) x1) x2) x3
          gfoldl k z (TildeP x1) = k (z TildeP) x1
          gfoldl k z (AsP x1 x2) = k (k (z AsP) x1) x2
          gfoldl k z (WildP) = z WildP
          gfoldl k z (RecP x1 x2) = k (k (z RecP) x1) x2
          gfoldl k z (ListP x1) = k (z ListP) x1
          gfoldl k z (SigP x1 x2) = k (k (z SigP) x1) x2



typename_Match = mkTyCon "Match"
instance Typeable Match
    where typeOf _ = mkTyConApp typename_Match ([])



instance Data Match
    where gfoldl k z (Match x1 x2 x3) = k (k (k (z Match) x1) x2) x3



typename_Clause = mkTyCon "Clause"
instance Typeable Clause
    where typeOf _ = mkTyConApp typename_Clause ([])



instance Data Clause
    where gfoldl k z (Clause x1 x2 x3) = k (k (k (z Clause) x1) x2) x3



typename_Exp = mkTyCon "Exp"
instance Typeable Exp
    where typeOf _ = mkTyConApp typename_Exp ([])



instance Data Exp
    where gfoldl k z (VarE x1) = k (z VarE) x1
          gfoldl k z (ConE x1) = k (z ConE) x1
          gfoldl k z (LitE x1) = k (z LitE) x1
          gfoldl k z (AppE x1 x2) = k (k (z AppE) x1) x2
          gfoldl k z (InfixE x1 x2 x3) = k (k (k (z InfixE) x1) x2) x3
          gfoldl k z (LamE x1 x2) = k (k (z LamE) x1) x2
          gfoldl k z (TupE x1) = k (z TupE) x1
          gfoldl k z (CondE x1 x2 x3) = k (k (k (z CondE) x1) x2) x3
          gfoldl k z (LetE x1 x2) = k (k (z LetE) x1) x2
          gfoldl k z (CaseE x1 x2) = k (k (z CaseE) x1) x2
          gfoldl k z (DoE x1) = k (z DoE) x1
          gfoldl k z (CompE x1) = k (z CompE) x1
          gfoldl k z (ArithSeqE x1) = k (z ArithSeqE) x1
          gfoldl k z (ListE x1) = k (z ListE) x1
          gfoldl k z (SigE x1 x2) = k (k (z SigE) x1) x2
          gfoldl k z (RecConE x1 x2) = k (k (z RecConE) x1) x2
          gfoldl k z (RecUpdE x1 x2) = k (k (z RecUpdE) x1) x2



typename_Body = mkTyCon "Body"
instance Typeable Body
    where typeOf _ = mkTyConApp typename_Body ([])



instance Data Body
    where gfoldl k z (GuardedB x1) = k (z GuardedB) x1
          gfoldl k z (NormalB x1) = k (z NormalB) x1



typename_Guard = mkTyCon "Guard"
instance Typeable Guard
    where typeOf _ = mkTyConApp typename_Guard ([])



instance Data Guard
    where gfoldl k z (NormalG x1) = k (z NormalG) x1
          gfoldl k z (PatG x1) = k (z PatG) x1



typename_Stmt = mkTyCon "Stmt"
instance Typeable Stmt
    where typeOf _ = mkTyConApp typename_Stmt ([])



instance Data Stmt
    where gfoldl k z (BindS x1 x2) = k (k (z BindS) x1) x2
          gfoldl k z (LetS x1) = k (z LetS) x1
          gfoldl k z (NoBindS x1) = k (z NoBindS) x1
          gfoldl k z (ParS x1) = k (z ParS) x1



typename_Range = mkTyCon "Range"
instance Typeable Range
    where typeOf _ = mkTyConApp typename_Range ([])



instance Data Range
    where gfoldl k z (FromR x1) = k (z FromR) x1
          gfoldl k z (FromThenR x1 x2) = k (k (z FromThenR) x1) x2
          gfoldl k z (FromToR x1 x2) = k (k (z FromToR) x1) x2
          gfoldl k z (FromThenToR x1
                                  x2
                                  x3) = k (k (k (z FromThenToR) x1) x2) x3



typename_Dec = mkTyCon "Dec"
instance Typeable Dec
    where typeOf _ = mkTyConApp typename_Dec ([])



instance Data Dec
    where gfoldl k z (FunD x1 x2) = k (k (z FunD) x1) x2
          gfoldl k z (ValD x1 x2 x3) = k (k (k (z ValD) x1) x2) x3
          gfoldl k z (DataD x1
                            x2
                            x3
                            x4
                            x5) = k (k (k (k (k (z DataD) x1) x2) x3) x4) x5
          gfoldl k z (NewtypeD x1
                               x2
                               x3
                               x4
                               x5) = k (k (k (k (k (z NewtypeD) x1) x2) x3) x4) x5
          gfoldl k z (TySynD x1 x2 x3) = k (k (k (z TySynD) x1) x2) x3
          gfoldl k z (ClassD x1
                             x2
                             x3
                             x4
                             x5) = k (k (k (k (k (z ClassD) x1) x2) x3) x4) x5
          gfoldl k z (InstanceD x1 x2 x3) = k (k (k (z InstanceD) x1) x2) x3
          gfoldl k z (SigD x1 x2) = k (k (z SigD) x1) x2
          gfoldl k z (ForeignD x1) = k (z ForeignD) x1



typename_FunDep = mkTyCon "FunDep"
instance Typeable FunDep
    where typeOf _ = mkTyConApp typename_FunDep ([])



instance Data FunDep
    where gfoldl k z (FunDep x1 x2) = k (k (z FunDep) x1) x2



typename_Foreign = mkTyCon "Foreign"
instance Typeable Foreign
    where typeOf _ = mkTyConApp typename_Foreign ([])



instance Data Foreign
    where gfoldl k z (ImportF x1
                              x2
                              x3
                              x4
                              x5) = k (k (k (k (k (z ImportF) x1) x2) x3) x4) x5
          gfoldl k z (ExportF x1
                              x2
                              x3
                              x4) = k (k (k (k (z ExportF) x1) x2) x3) x4



typename_Callconv = mkTyCon "Callconv"
instance Typeable Callconv
    where typeOf _ = mkTyConApp typename_Callconv ([])



instance Data Callconv
    where gfoldl k z (CCall) = z CCall
          gfoldl k z (StdCall) = z StdCall



typename_Safety = mkTyCon "Safety"
instance Typeable Safety
    where typeOf _ = mkTyConApp typename_Safety ([])



instance Data Safety
    where gfoldl k z (Unsafe) = z Unsafe
          gfoldl k z (Safe) = z Safe
          gfoldl k z (Threadsafe) = z Threadsafe



typename_Strict = mkTyCon "Strict"
instance Typeable Strict
    where typeOf _ = mkTyConApp typename_Strict ([])



instance Data Strict
    where gfoldl k z (IsStrict) = z IsStrict
          gfoldl k z (NotStrict) = z NotStrict



typename_Con = mkTyCon "Con"
instance Typeable Con
    where typeOf _ = mkTyConApp typename_Con ([])



instance Data Con
    where gfoldl k z (NormalC x1 x2) = k (k (z NormalC) x1) x2
          gfoldl k z (RecC x1 x2) = k (k (z RecC) x1) x2
          gfoldl k z (InfixC x1 x2 x3) = k (k (k (z InfixC) x1) x2) x3
          gfoldl k z (ForallC x1 x2 x3) = k (k (k (z ForallC) x1) x2) x3



typename_Type = mkTyCon "Type"
instance Typeable Type
    where typeOf _ = mkTyConApp typename_Type ([])



instance Data Type
    where gfoldl k z (ForallT x1
                              x2
                              x3) = k (k (k (z ForallT) x1) x2) x3
          gfoldl k z (VarT x1) = k (z VarT) x1
          gfoldl k z (ConT x1) = k (z ConT) x1
          gfoldl k z (TupleT x1) = k (z TupleT) x1
          gfoldl k z (ArrowT) = z ArrowT
          gfoldl k z (ListT) = z ListT
          gfoldl k z (AppT x1 x2) = k (k (z AppT) x1) x2
#endif
