module Data.Derive.Example(Data2(..), guess) where

import Language.Haskell.TH hiding (ppr)
import Data.Derive.FixedPpr
import Data.List


data Data2 a b = Ctor0
               | Ctor1 a
               | Ctor2 a b


guess :: Q [Dec] -> IO ()
guess x = runQ x >>= putStr . unlines . map guessDec


guessDec :: Dec -> String
guessDec (InstanceD ctx typ inner) =
    "instance_context " ++ guessContext ctx ++ " " ++
    guessPrinciple typ ++ " " ++ list (map guessDec inner)

guessDec x = error $ show x



guessContext = list . nub . map guessPrinciple

guessPrinciple (AppT (ConT x) _) = show $ dropModule $ show x



dropModule = reverse . takeWhile (/= '.') . reverse

list x = "[" ++ concat (intersperse "," x) ++ "]"
