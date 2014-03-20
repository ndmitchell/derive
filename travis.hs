
import Neil

main = do
    retry 3 $ cmd "cabal install cereal json binarydefer data-lens QuickCheck"
    cmd "runhaskell Main --generate"
    cmd "runhaskell Main --test"
