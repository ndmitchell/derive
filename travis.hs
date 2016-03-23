
import System.Process.Extra
import Control.Exception.Extra
import System.Environment.Extra
import System.Exit

main = do
    putStrLn "DEBUG: Running travis.hs test script"
    ver <- lookupEnv "GHCVER"
    system "ghc-pkg init" -- may fail
    (\act -> if ver == Just "head" then catch_ act $ const exitSuccess else act) $
        retry 3 $ system_ "cabal install cereal json binarydefer QuickCheck --force-reinstalls"
    system_ "runhaskell Main --generate"
    system_ "git diff --exit-code"
    system_ "runhaskell Main --test"
