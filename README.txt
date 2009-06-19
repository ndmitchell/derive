Derive README
-------------

This README corresponds to the source distribution only.

There are derivations that are derived from example, and those which are derived automatically


Data.Derive.SYB.Foo is the SYB generator
Data.Derive.Foo is the TH generator and optionally a DSL value

Modules intended for the user are:

Data.DeriveSYB -- Data a => a -> [HSE.Decl]
Data.DeriveDSL -- deriveDSL :: [HSE.Decl] -> DSL ; applyDSL :: DSL -> HSE.Decl -> [HSE.Decl]
Data.DeriveTH -- derive :: TH.DataType -> [TH.Decl]


Data.Derive = imports all exports all the other ones



makeEq :: Derivation
dslEq :: DSL


Some are auto-generated with:


