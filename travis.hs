
import Neil

main = do
    retry 3 $ cmd "cabal install cereal json binarydefer data-lens QuickCheck --force-reinstalls"
    cmd "runhaskell Main --generate"
    cmd "git checkout > temp.txt" -- should have no stdout
    src <- readFile "temp.txt"
    print src
    when (lines src /= []) $ error "generating changed something!"
    cmd "runhaskell Main --test"
