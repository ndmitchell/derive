
import System.Process.Extra
import Control.Exception.Extra

main = do
    retry 3 $ system_ "cabal install cereal json binarydefer data-lens QuickCheck --force-reinstalls"
    system_ "runhaskell Main --generate"
    system_ "git diff --exit-code"
    system_ "runhaskell Main --test"
