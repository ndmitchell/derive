
import Neil

main = do
    retry 3 $ cmd "cabal install cereal json binarydefer data-lens QuickCheck --force-reinstalls"
    cmd "runhaskell Main --generate"
    cmd "runhaskell Main --test"
