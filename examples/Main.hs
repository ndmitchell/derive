import Example
import Data.Derive.Eq
import Data.Derive.Ord
import Data.Derive.Binary
import Data.Derive.BinaryDefer
import Data.Derive.Functor
import Data.Derive.SYB

main = do f (undefined :: Foo)
          f (undefined :: Color)
          f (undefined :: Computer)
          f (undefined :: Drinks)
          f (undefined :: FailList A B)
          f (undefined :: State    A B)
    where
        f x = mapM_ (\y -> putStrLn (derive y x ++ "\n"))
              [ makeEq, makeBinary, makeBinaryDefer, makeFunctor ]
