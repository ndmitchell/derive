{-# OPTIONS_DERIVE --derive=Data,Eq ,Typeable,Eq,Ord #-}

module Example where


data Foo = Bar


data Color = RGB Int Int Int
           | CMYK Int Int Int Int


data Computer = Laptop { weight :: Int }
              | Desktop { speed :: Int, memory :: Int }


-- | All drinks mankind will ever need
data Drinks = Beer Bool{-ale?-}
            | Coffee
            | Tea
            | EnergyDrink
            | Water
            | Wine
            | Whisky


-- | A list with late failure
data FailList e a = Nil | Fail e | Const a (FailList e a)
                    deriving ( {- ! Functor !-} )


-- | State monad
data State s a = StateT (s -> (s, a))
                 deriving ( {- ! Functor !-} )

