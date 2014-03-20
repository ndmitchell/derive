
import Control.Monad
import System.Cmd
import System.Exit


main = do
    cmd "cabal install cereal json binarydefer data-lens QuickCheck"
    cmd "runhaskell Main --generate"
    cmd "runhaskell Main --test"


cmd :: String -> IO ()
cmd x = do
    res <- system x
    when (res /= ExitSuccess) $ error $ "Failed in system command: " ++ x
