
module Data.Derive.Play(makePlay) where

import Data.Derive
import Data.List
import Data.Derive.FixedPpr
import Control.Monad.State
import Language.Haskell.TH.Syntax



makePlay :: Derivation
makePlay = Derivation derive "Play"


data Container = None | Item
               | Container String [Container]
               | Var String Container
                 deriving (Eq, Show)


typeToContainer :: String -> RType -> Container
typeToContainer active (RType (TypeCon x) xs)
    | x == active = Item
    | otherwise = if all (== None) ys then None else Container x ys
        where ys = map (typeToContainer active) xs
typeToContainer _ _ = None


addNames :: Container -> Container
addNames (Container name xs) = Container name $ evalState (mapM f xs) ['x' : show i | i <- [1..]]
    where
        f None = return None
        f Item = getName >>= \y -> return $ Var y Item
        f o@(Container "[]" _) = getName >>= \y -> return $ Var y o
        f x = error $ "Don't know how to derive Play for: " ++ show x

        getName = do (y:ys) <- get; put ys; return y



derive dat@(DataDef name arity ctors) =
        simple_instance "Play" dat [funN "getChildren" gbody] -- , funN "replaceChildren" rbody]
    where
        gbody = [gitem (addNames $ Container (ctorName ctor) $ map (typeToContainer name) (ctorTypes ctor))
                | ctor <- ctors]
        
        gitem x = sclause [lhs x] (rhs x) -- f x) (lit (0 :: Integer)) (lit (0 :: Integer)) -- error $ show x -- ctor = error $ show (ctor, map (typeToContainer name) (ctorTypes ctor))
            where
                lhs None = WildP
                lhs (Container x xs) = lK x (map lhs xs)
                lhs (Var x _) = vr x
                
                rhs (Container _ xs) = if null res then l0 "[]" else fromEither $ foldr1 f res
                    where res = concatMap rhss xs
                
                fromEither (Right x) = x
                fromEither (Left  x) = l2 ":" x (l0 "[]")
                
                f (Left  x) y = Right $ l2 ":"  x (fromEither y)
                f (Right x) y = Right $ l2 "++" x (fromEither y)
                
                -- return Left for an item, Right for a list
                rhss (Container _ xs) = concatMap rhss xs
                rhss None = []
                rhss (Var x Item) = [Left (vr x)]
                rhss (Var x (Container "[]" [Item])) = [Right (vr x)]
                rhss x = error $ "Right hand side of Play not handled, " ++ show x

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
