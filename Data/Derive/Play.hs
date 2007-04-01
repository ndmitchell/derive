
module Data.Derive.Play(makePlay) where

import Data.Derive
import Data.Derive.Peephole
import Data.List
import Data.Maybe
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


everything :: Container -> [Container]
everything o@(List x) = o : everything x
everything o@(Tuple x) = o : concatMap everything x
everything o = [o]


derive dat@(DataDef name arity ctors) = peephole $
        simple_instance "Play" dat [funN "getChildren" gbody, funN "replaceChildren" rbody]
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
                f (Tuple xs) = LamE [tup (map var ns)]
                                    (concat' [AppE (f x) (var n) | (n,x) <- zip ns xs])
                    where ns = [1..length xs]

        rbody = [Clause [vr "x"] (NormalB bod) (r:k:map lst lsts)]
            where
                lsts :: [(Container,Int)]
                lsts = zip (nub [x | List x <- concatMap (concatMap everything . contain) ctors]) [1..]
                
                lst (x,i) = funN ("lst" ++ show i)
                    [sclause [nil, vr "xs"] $ tup [nil, vr "xs"]
                    ,sclause [cons (vr "c") (vr "cs"), vr "xs"] $
                        LetE [sval (tup [vr "a", vr "b"]) (AppE (ritem lsts x) (vr "c"))
                             ,sval (tup [vr "q", vr "u"]) (l2 ("lst" ++ show i) (vr "cs") (vr "b"))
                             ]
                             (tup [cons (vr "a") (vr "q"), vr "u"])
                    ]
            
                bod = case' (vr "x") [(ctp ctor 'x',
                    tup [gitem cs, l2 "." (l0 "fst") $ rs (ctorName ctor) (map (ritem lsts) cs)]
                    ) | ctor <- ctors, let cs = contain ctor]
                
                r = funN "r" [sclause [vr "c", vr "xs"] $ tup [vr "c", vr "xs"]]
                k = funN "k" [sclause [vr "f", vr "g", vr "xs"] $
                        LetE [sval (tup [vr "a", vr "b"]) (AppE (vr "f") (vr "xs"))
                             ,sval (tup [vr "c", vr "d"]) (AppE (vr "g") (vr "b"))
                             ]
                             (tup [AppE (vr "a") (vr "c"), vr "d"])
                        ]

        ritem :: [(Container,Int)] -> Container -> Exp
        ritem lsts None = LamE [vr "c",vr "xs"] (tup [vr "c",vr "xs"])
        ritem lsts Target = LamE [vr "c",cons (vr "x") (vr "xs")] (tup [vr "x",vr "xs"])
        ritem lsts (List x) = l0 $ "lst" ++ show (fromJust $ lookup x lsts)
        
        ritem lsts _ = l0 "todo"


        rs c xs = foldl f (l1 "r" (l0 c)) xs
            where f x y = AppE (AppE (vr "k") x) y


        

        --ritem :: Ctor -> (Exp, [Decl])
        --ritem conts = 




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
