-- NOTE: Cannot be guessed as it relies on type information

module Data.Derive.Play(makePlay) where

import Language.Haskell.TH.All
import Data.List
import Data.Maybe
import Data.Generics
import qualified Data.Map as Map
import Control.Monad.State
import Debug.Trace



makePlay :: Derivation
makePlay = Derivation play' "Play"


data Container = None | Target
               | List Container | Tuple [Container]
                 deriving (Eq, Show)


-- convert a type to the appropriate container type
typeToContainer :: String -> Type -> Container
typeToContainer active t =
        if eqConT active name then Target
        else if all (== None) rest2 then None
        else if name == ListT then List (head rest2)
        else if isTupleT name then Tuple rest2
        else error $ "Play derivation on unknown type: " ++ show t
    where
        (name,rest) = typeApp t
        rest2 = map (typeToContainer active) rest


-- the variable type
type Var x = State (Map.Map String Int) x

getVar :: String -> Var String
getVar x = do mp <- get
              let i = Map.findWithDefault 1 x mp
              put $ Map.insert x (i+1) mp
              return $ x ++ if i == 1 then "" else show i

runVar :: Var a -> a
runVar x = evalState x Map.empty



play' dat =
        [instance_default "Play" dat [funN "getChildren" gbody, funN "replaceChildren" rbody]]
    where
        ctors :: [(CtorDef,[Container])]
        ctors = [(c, map (typeToContainer (dataName dat)) (ctorTypes c)) | c <- dataCtors dat]


        gbody = [sclause [ctp (fst c) 'x'] (gitem c) | c <- ctors]

        gitem :: (CtorDef,[Container]) -> Exp
        gitem (c,ts) = (++::) [AppE (f t) v | (t,v) <- zip ts (ctv c 'x')]
            where
                f None = const' nil
                f Target = LamE [vr "x"] (box (vr "x"))
                f (List t) = l1 "concatMap" (f t)
                f (Tuple ts) = LamE [tup (map vr vars)] ((++::) [AppE (f t) (vr v) | (t,v) <- zip ts vars])
                    where vars = ['x':show i | i <- [1..length ts]]


        rbody = [Clause [vr "x"] (NormalB $ case' (vr "x")
                    [(ctp (fst c) 'x', tup [gitem c, ritem c]) | c <- ctors]
                ) [funcList]]
        
        
        funcList = funN "list'"
                        [sclause [vr "f",nil] (runVar $ const1_ nil)
                        ,sclause [vr "f",lK ":" [vr "x",vr "xs"]]
                            (runVar $ do
                                a <- const1_ (l0 ":")
                                lhs <- join_ a (l1 "f" (vr "x"))
                                join_ lhs (lK "list'" [vr "f",vr "xs"])
                        )]

        
        wildcard :: Exp -> Exp
        wildcard x = everywhere (mkT f) x
            where
                vars = everything (++) ([] `mkQ` g) x
                
                g (VarE x) = [x]
                g x = []
                
                f (VarP x) | x `notElem` vars = WildP
                f x = x
        
        
        ritem :: (CtorDef,[Container]) -> Exp
        ritem (c,ts) = wildcard $ runVar (value_ $ joins_ (l0 (ctorName c)) items)
            where
                items = zip ts (ctv c 'x')
            
                f None = const_
                f Target = id_
                f (List t) = list_ (f t)
                f (Tuple ts) = do
                    vars1 <- replicateM (length ts) (getVar "x")
                    vars2 <- replicateM (length ts) (getVar "y")
                    bod <- joins_ (LamE (map vr vars1) (tup $ map vr vars1)) (zip ts (map vr vars2))
                    return $ LamE [tup $ map vr vars2] bod

                joins_ root xs = do
                    root2 <- const1_ root
                    xs2 <- mapM (\(t,v) -> liftM (`appE` v) (f t)) xs
                    foldM join_ root2 xs2


       -- the replaceChildren combinators
        
        const_ = do
            y <- getVar "y"
            ns <- getVar "ns"
            c <- getVar "c"
            return $ lamE [vr y,vr ns,vr c] (app (vr c) [vr y,vr ns])

        id_ = do
            n <- getVar "n"
            ns <- getVar "ns"
            c <- getVar "c"
            return $ lamE [WildP,lK ":" [vr n,vr ns],vr c] (app (vr c) [vr n,vr ns])
        
        isId (LamE [WildP,ConP cons [VarP n, VarP ns],VarP c] (AppE (AppE (VarE c2) (VarE n2)) (VarE ns2)))
            | show cons == ":" && n == n2 && c == c2 && ns == ns2 = True
        isId _ = False
        
        list_ x = liftM (l1 "list'") x
        
        value_ inp = do
            inp <- inp
            ns <- getVar "ns"
            y <- getVar "y"
            return $ lamE [vr ns] $ appE2 inp (vr ns) $ lamE [vr y,WildP] (vr y)

        join_ ina inb = do
            ns1 <- getVar "ns"
            ns2 <- getVar "ns"
            ns3 <- getVar "ns"
            c <- getVar "c"
            a <- getVar "a"
            b <- getVar "b"
            return $ lamE [vr ns1,vr c]
                   $ appE2 ina (vr ns1) $ lamE [vr a,vr ns2]
                   $ appE2 inb (vr ns2) $ lamE [vr b,vr ns3]
                   $ appE2 (vr c) (appE (vr a) (vr b)) (vr ns3)

        const1_ x = do c <- const_ ; return $ appE c x



        -- can be a bit more fast and loose about variable clashes etc
        -- since we guarantee a limited set of generated values

        -- very special rule, only valid for play
        appE (AppE (AppE (AppE (VarE l) i) x) y) (LamE (b:WildP:bs) c)
            | show l == "list'" && isId i = appE (lamE (b:bs) c) y

        appE (LamE (VarP x:xs) y) z = lamE xs (rebuild $ replaceVar x z y)
        appE (LamE (WildP :xs) y) z = lamE xs y
        appE (AppE (LamE (x1:VarP x2:xs) xb) y) z =
              AppE (lamE (x1:xs) (rebuild $ replaceVar x2 z xb)) y
        appE x y = AppE x y
        
        
        
        
        appE2 x y z = appE (appE x y) z
        
        lamE [] y = y
        lamE xs (LamE ys z) = lamE (xs++ys) z
        lamE xs (AppE y (VarE z)) | last xs == VarP z = lamE (init xs) y
        lamE x  y = LamE x y

        rebuild :: Exp -> Exp
        rebuild = everywhere (mkT f)
            where
                f (AppE x y) = appE x y
                f (LamE x y) = lamE x y
                f x = x
