
module Main where

import HSE
import Apply
import DSL
import Derive
import Data.List

main = do
    ParseOk (Module _ _ _ _ _ _ decls) <- parseFile "Examples.hs"
    let todo = [(name, takeWhile (not . isUnknownDeclPragma) real) | UnknownDeclPragma _ "DERIVE" name:real <- tails decls]
    mapM_ (uncurry tester) todo


tester :: String -> Out -> IO ()
tester name out = do
    putStrLn $ "Testing for " ++ name
    putStr   $ showOut out
    putStrLn $ showOut $ simplifyOut $ apply list $ head $ derive out
