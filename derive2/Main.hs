
module Main where

import HSE
import Apply
import DSL
import Guess
import Data.List

main = do
    ParseOk (Module _ _ _ _ _ _ decls) <- parseFile "Examples.hs"
    let todo = [(name, takeWhile (not . isUnknownDeclPragma) real) | UnknownDeclPragma _ "DERIVE" name:real <- tails decls]
    mapM_ (uncurry tester) todo


tester :: String -> Res -> IO ()
tester name res = do
    putStrLn $ "Testing for " ++ name
    putStr   $ showRes res
    putStrLn $ showRes $ simplifyRes $ apply dataTypeList $ head $ guess res
