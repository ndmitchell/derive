Derive README
-------------

This README corresponds to the source distribution only, and contains notes to help get
code contributors working.

The standard sequence for testing derive is:

$ ghci Main.hs
> :main --generate
> :r
> :main --test

The --generate option will automatically generate DSL's for derivations derived by example.
The --test option runs all test comparisons and then loads the file with Template Haskell.
