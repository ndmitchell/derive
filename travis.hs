
import Neil

main = do
    cmd "cabal install cereal json binarydefer data-lens QuickCheck --verbose"
    cmd "runhaskell Main --generate"
    cmd "runhaskell Main --test"
