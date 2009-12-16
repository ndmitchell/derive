{-# OPTIONS_DERIVE --derive=Eq,Ord,Eq #-}

module Example where


data Foo = Bar


data Color = RGB Int Int Int
           | CMYK Int Int Int Int


data Computer = Laptop { weight :: Int }
              | Desktop { speed :: Int, memory :: Int }

-- | State monad
data State s a = StateT (s -> (s, a))
                 deriving ( {-! Functor !-} )


{-!
deriving instance Show Color
!-}
