import Example
import Data.Derive.StdDerivations
import Data.Derive.Driver

main = do f (undefined :: Foo)
          f (undefined :: Color)
          f (undefined :: Computer)
          f (undefined :: Drinks)
          f (undefined :: FailList A B)
          f (undefined :: State    A B)
    where
        f x = mapM_ (\y -> derive y x >> putStrLn "") derivable
