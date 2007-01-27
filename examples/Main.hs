import Example
import Data.Derive.Driver

main = do f (undefined :: Foo)
          f (undefined :: Color)
          f (undefined :: Computer)
          f (undefined :: Drinks)
    where
        f x = mapM_ (\y -> derive y x >> putStrLn "") derivable
