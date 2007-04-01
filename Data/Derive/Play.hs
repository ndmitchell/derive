
module Data.Derive.Play(makePlay) where

import Data.Derive
import Data.Derive.Peephole
import Data.List
import Data.Derive.FixedPpr
import Control.Monad.State
import Language.Haskell.TH.Syntax



makePlay :: Derivation
makePlay = Derivation derive "Play"


data Container = None | Target
               | List Container | Tuple [Container]
                 deriving (Eq, Show)


typeToContainer :: String -> RType -> Container
typeToContainer active t@(RType (TypeCon x) xs)
    | x == active = Target
    | otherwise = if all (== None) ys then None
                  else if x == "[]" then List (head ys)
                  else if all (== ',') x then Tuple ys
                  else error $ "Play derivation on unknown type: " ++ show t
        where ys = map (typeToContainer active) xs
typeToContainer _ _ = None



derive dat@(DataDef name arity ctors) = peephole $
        simple_instance "Play" dat [funN "getChildren" gbody] -- , funN "replaceChildren" rbody]
    where
        contain = map (typeToContainer name) . ctorTypes
        var x = vr $ 'x' : show x
        match ctor = sclause [ctp ctor 'x']
    
        gbody = [match ctor (gitem $ contain ctor) | ctor <- ctors]
        
        gitem :: [Container] -> Exp
        gitem conts = concat' [AppE (f c) (var i) | (i,c) <- zip [1..] conts]
            where
                f None = l1 "const" nil
                f Target = LamE [var 1] (box (var 1))
                f (List x) = l1 "concatMap" (f x)
                f (Tuple xs) = LamE [lK (replicate (length xs - 1) ',') (map var ns)]
                                    (concat' [AppE (f x) (var n) | (n,x) <- zip ns xs])
                    where ns = [1..length xs]

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
