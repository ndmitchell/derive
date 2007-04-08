{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_DERIVE --derive=Typeable,Data --output=SYB.hs #-}

{-
NOTE: This is a copy of the template haskell syntax, modified to allow
derive to work over it, and so that we can derive SYB instances
-}

module Language.Haskell.TH.Semtex where

import Data.PackedString
import GHC.Base     ( Int(..), Int#, (<#), (==#) )


type OccName = PackedString
type ModName = PackedString -- Module name
type PkgName = PackedString -- package name


data Name = Name OccName NameFlavour
    deriving (Eq,Ord,Show)

data NameFlavour
  = NameS           -- An unqualified name; dynamically bound
  | NameQ ModName       -- A qualified name; dynamically bound

  | NameU Int#          -- A unique local name

    -- The next two are for lexically-scoped names that
    -- are bound *outside* the TH syntax tree, 
    -- either globally (NameG) or locally (NameL)
    -- e.g. f x = $(h [| (map, x) |]
    --      The 'map' will be a NameG, and 'x' wil be a NameL
    -- These Names should never appear in a binding position in a TH syntax tree

  | NameL Int#          -- 
  | NameG NameSpace PkgName ModName -- An original name (occurrences only, not binders)
                -- Need the namespace too to be sure which 
                -- thing we are naming
    deriving (Eq,Ord,Show)

data NameSpace = VarName    -- Variables
           | DataName   -- Data constructors 
           | TcClsName  -- Type constructors and classes; Haskell has them
                -- in the same name space for now.
           deriving( Eq, Ord , Show)

type Uniq = Int

----------------------------------------------------
--
--  The Info returned by reification
--
-----------------------------------------------------

data Info 
  = ClassI Dec
  | ClassOpI
    Name    -- The class op itself
    Type    -- Type of the class-op (fully polymoprhic)
    Name    -- Name of the parent class
    Fixity

  | TyConI Dec

  | PrimTyConI  -- Ones that can't be expressed with a data type 
        -- decl, such as (->), Int#
    Name 
    Int -- Arity
    Bool    -- False => lifted type; True => unlifted

  | DataConI 
    Name    -- The data con itself
    Type    -- Type of the constructor (fully polymorphic)
    Name    -- Name of the parent TyCon
    Fixity

  | VarI 
    Name    -- The variable itself
    Type 
    (Maybe Dec) -- Nothing for lambda-bound variables, and 
            -- for anything else TH can't figure out
            -- E.g. [| let x = 1 in $(do { d <- reify 'x; .. }) |]
    Fixity

  | TyVarI  -- Scoped type variable
    Name
    Type    -- What it is bound to
  deriving( Show )

data Fixity          = Fixity Int FixityDirection deriving( Eq, Show )
data FixityDirection = InfixL | InfixR | InfixN   deriving( Eq, Show )

maxPrecedence :: Int
maxPrecedence = (9::Int)

defaultFixity :: Fixity
defaultFixity = Fixity maxPrecedence InfixL


-----------------------------------------------------
--
--  The main syntax data types
--
-----------------------------------------------------

data Lit = CharL Char 
         | StringL String 
         | IntegerL Integer     -- Used for overloaded and non-overloaded
                                -- literals. We don't have a good way to
                                -- represent non-overloaded literals at
                                -- the moment. Maybe that doesn't matter?
         | RationalL Rational   -- Ditto
         | IntPrimL Integer
         | FloatPrimL Rational
         | DoublePrimL Rational
    deriving( Show, Eq )

    -- We could add Int, Float, Double etc, as we do in HsLit, 
    -- but that could complicate the
    -- suppposedly-simple TH.Syntax literal type

data Pat 
  = LitP Lit                      -- { 5 or 'c' }
  | VarP Name                   -- { x }
  | TupP [Pat]                    -- { (p1,p2) }
  | ConP Name [Pat]             -- data T1 = C1 t1 t2; {C1 p1 p1} = e 
  | InfixP Pat Name Pat           -- foo ({x :+ y}) = e 
  | TildeP Pat                    -- { ~p }
  | AsP Name Pat                -- { x @ p }
  | WildP                         -- { _ }
  | RecP Name [FieldPat]        -- f (Pt { pointx = x }) = g x
  | ListP [ Pat ]                 -- { [1,2,3] }
  | SigP Pat Type                 -- p :: t
  deriving( Show, Eq )

type FieldPat = (Name,Pat)

data Match = Match Pat Body [Dec]
                                    -- case e of { pat -> body where decs } 
    deriving( Show, Eq )
data Clause = Clause [Pat] Body [Dec]
                                    -- f { p1 p2 = body where decs }
    deriving( Show, Eq )
 
data Exp 
  = VarE Name                        -- { x }
  | ConE Name                        -- data T1 = C1 t1 t2; p = {C1} e1 e2  
  | LitE Lit                           -- { 5 or 'c'}
  | AppE Exp Exp                       -- { f x }

  | InfixE (Maybe Exp) Exp (Maybe Exp) -- {x + y} or {(x+)} or {(+ x)} or {(+)}
    -- It's a bit gruesome to use an Exp as the
    -- operator, but how else can we distinguish
    -- constructors from non-constructors?
    -- Maybe there should be a var-or-con type?
    -- Or maybe we should leave it to the String itself?

  | LamE [Pat] Exp                     -- { \ p1 p2 -> e }
  | TupE [Exp]                         -- { (e1,e2) }  
  | CondE Exp Exp Exp                  -- { if e1 then e2 else e3 }
  | LetE [Dec] Exp                     -- { let x=e1;   y=e2 in e3 }
  | CaseE Exp [Match]                  -- { case e of m1; m2 }
  | DoE [Stmt]                         -- { do { p <- e1; e2 }  }
  | CompE [Stmt]                       -- { [ (x,y) | x <- xs, y <- ys ] }
  | ArithSeqE Range                    -- { [ 1 ,2 .. 10 ] }
  | ListE [ Exp ]                      -- { [1,2,3] }
  | SigE Exp Type                      -- e :: t
  | RecConE Name [FieldExp]            -- { T { x = y, z = w } }
  | RecUpdE Exp [FieldExp]             -- { (f x) { z = w } }
  deriving( Show, Eq )

type FieldExp = (Name,Exp)

-- Omitted: implicit parameters

data Body
  = GuardedB [(Guard,Exp)]   -- f p { | e1 = e2 | e3 = e4 } where ds
  | NormalB Exp              -- f p { = e } where ds
  deriving( Show, Eq )

data Guard
  = NormalG Exp
  | PatG [Stmt]
  deriving( Show, Eq )

data Stmt
  = BindS Pat Exp
  | LetS [ Dec ]
  | NoBindS Exp
  | ParS [[Stmt]]
  deriving( Show, Eq )

data Range = FromR Exp | FromThenR Exp Exp
           | FromToR Exp Exp | FromThenToR Exp Exp Exp
          deriving( Show, Eq )
  
data Dec 
  = FunD Name [Clause]            -- { f p1 p2 = b where decs }
  | ValD Pat Body [Dec]           -- { p = b where decs }
  | DataD Cxt Name [Name] 
         [Con] [Name]             -- { data Cxt x => T x = A x | B (T x)
                                  --       deriving (Z,W)}
  | NewtypeD Cxt Name [Name] 
         Con [Name]               -- { newtype Cxt x => T x = A (B x)
                                  --       deriving (Z,W)}
  | TySynD Name [Name] Type       -- { type T x = (x,x) }
  | ClassD Cxt Name [Name] [FunDep] [Dec]
                                  -- { class Eq a => Ord a where ds }
  | InstanceD Cxt Type [Dec]      -- { instance Show w => Show [w]
                                  --       where ds }
  | SigD Name Type                -- { length :: [a] -> Int }
  | ForeignD Foreign
  deriving( Show, Eq )

data FunDep = FunDep [Name] [Name]
  deriving( Show, Eq )

data Foreign = ImportF Callconv Safety String Name Type
             | ExportF Callconv        String Name Type
         deriving( Show, Eq )

data Callconv = CCall | StdCall
          deriving( Show, Eq )

data Safety = Unsafe | Safe | Threadsafe
        deriving( Show, Eq )

type Cxt = [Type]    -- (Eq a, Ord b)

data Strict = IsStrict | NotStrict
         deriving( Show, Eq )

data Con = NormalC Name [StrictType]
         | RecC Name [VarStrictType]
         | InfixC StrictType Name StrictType
         | ForallC [Name] Cxt Con
         deriving( Show, Eq )

type StrictType = (Strict, Type)
type VarStrictType = (Name, Strict, Type)

-- FIXME: Why this special status for "List" (even tuples might be handled
--      differently)? -=chak
data Type = ForallT [Name] Cxt Type   -- forall <vars>. <ctxt> -> <type>
          | VarT Name                 -- a
          | ConT Name                 -- T
          | TupleT Int                -- (,), (,,), etc.
          | ArrowT                    -- ->
          | ListT                     -- []
          | AppT Type Type            -- T a b
      deriving( Show, Eq )

