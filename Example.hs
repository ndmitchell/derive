{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}
{-# OPTIONS_DERIVE --derive=Eq,Ord #-}

module Example where

data Foo = Bar

data Color = RGB Int Int Int
           | CMYK Int Int Int Int

data Computer = Laptop { weight :: Int }
              | Desktop { speed :: Int, memory :: Int }
                deriving ( {-! Show !-} )

{-!
deriving instance Show Color
!-}

main = print (RGB 1 2 3 == CMYK 1 2 3 4, RGB 1 2 3)

