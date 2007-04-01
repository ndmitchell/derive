
module Data.Derive.Play(makePlay) where

import Data.Derive
import Data.List
import Data.Derive.FixedPpr
import Control.Monad.State
import Language.Haskell.TH.Syntax



makePlay :: Derivation
makePlay = Derivation derive "Play"


data Container = None | Target
               | Container String [Container]
                 deriving (Eq, Show)


typeToContainer :: String -> RType -> Container
typeToContainer active (RType (TypeCon x) xs)
    | x == active = Target
    | otherwise = if all (== None) ys then None else Container x ys
        where ys = map (typeToContainer active) xs
typeToContainer _ _ = None



derive dat@(DataDef name arity ctors) =
        simple_instance "Play" dat [funN "getChildren" gbody] -- , funN "replaceChildren" rbody]
    where
        contain = map (typeToContainer name) . ctorTypes
        var = vr . ('x':) . show
        match ctor = sclause [ctp ctor 'x']
    
        gbody = [match ctor (gitem $ contain ctor) | ctor <- ctors]
        
        gitem :: [Container] -> Exp
        gitem conts = concat' [AppE (f c) (var i) | (i,c) <- zip [1..] conts]
            where
                f None = l1 "const" nil
                f Target = l0 "id"
                f (Container "[]" [x]) = l1 "map" (f x)
                f (Container "," [x,None]) = l2 "." (l0 "fst") (f x)
                f (Container "," [None,x]) = l2 "." (l0 "snd") (f x)
                f (Container "," [x,y]) = LamE [l2 "," (vr "t1") (vr "t2")]
                                          (AppE (f x) (vr "t1") ++: AppE (f y) (vr "t2"))
                
                -- = l2 "concatMap" [Right $ var i]
                --f i (Container "[]" [x]) = [Right $ var i]
                --f i (Container "," [None,Target]) = [Right $ l2 "map" (l0 "snd") (var i)]
                
                f x = error $ "Play.gitem, unhandled case: " ++ show x
                
                

{-                
                -- return Left for an item, Right for a list
                rhss (Container _ xs) = concatMap rhss xs
                rhss None = []
                rhss (Var x Target) = [Left (vr x)]
                rhss (Var x (Container "[]" [Target])) = [Right (vr x)]
                rhss x = error $ "Right hand side of Play not handled, " ++ show x
-}



{-        
        
        -- sequ' (ptag (lit (0::Integer)) : map (l1 "put") (ctv ctor 'x'))
        

    
        rbody = [ sclause [ctp ctor 'x'] (put_case nm ctor) | (nm,ctor) <- items ]
        put_case nm ctor = sequ' (ptag (lit nm) : map (l1 "put") (ctv ctor 'x'))

        dbody = [sclause [] (gtag >>=: ("tag_" ->: case' (vr "tag_") (map get_case items)))]
        get_case (nm,ctor) = (lit nm, liftmk (ctc ctor) (replicate (ctorArity ctor) (vr "get")))

        nctors = length ctors
        items :: [(Integer,CtorDef)]
        items = zip [0..] ctors

        (ptag, gtag) | nctors <= 1     = (\_ -> l1 "return" (lit ()), l1 "return" (lit (0::Integer)))
                     | nctors <= 256   = (l1 "putWord8", l0 "getWord8")
                     | nctors <= 65536 = (l1 "putWord16", l0 "getWord16")
                     | otherwise       = (l1 "putWord32", l0 "getWord32")
-}
