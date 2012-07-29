Derive README
-------------

This README corresponds to the source distribution only, and contains notes to help get
code contributors working.

The standard sequence for testing derive is:

$ ghci Main.hs
> :main --generate
> :reload
> :main --test

The --generate option will automatically generate DSL's for derivations derived by example.
The --test option runs all test comparisons and then loads the file with Template Haskell.


Adding New Derivations
----------------------

(This section is incomplete, and possibly wrong. Please submit patches.)

My best suggestion, start with a similar instance, i.e. to make Eq2 from Eq do:

* Copy Data/Derive/Eq.hs to Data/Derive/Eq2.hs
* Rename some of the bits in Eq2.hs from Eq
* ghci                -- load derive
* :main --generate    -- this adds Eq2.hs to the .cabal/All.hs files etc
* :reload             -- reload with Eq2.hs

Now fix up Eq2.hs appropriately.
