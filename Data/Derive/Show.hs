
-- | Derives @Show@.  This is as defined by the Haskell report, except
-- there is no support for infix constructors.  If you attempt to
-- derive @Show@ for a data type with infix constructors, the
-- constructors are handled as if they were prefix constructors, using
-- the @(@/consym/@)@ syntax.
module Data.Derive.Show(makeShow) where

import Language.Haskell.TH.All
import Data.List

makeShow :: Derivation
makeShow = Derivation show' "Show"

show' dat = [instance_default "Show" dat [funN "showsPrec" body]]
    where
        body = [ sclause [vr "p", ctp ctr 'x'] (showit ctr) | ctr <- dataCtors dat ]

showit ctr = case ctorFields ctr of [] -> norm
                                    fl -> flds fl
    where
        norm = l2 "showParen" (vr "p" >: lit (10::Integer))
               ((.::) (intersperse (scl ' ')
                       (ssl pname : map (l2 "showsPrec" (lit (11::Integer))) (ctv ctr 'x'))))

        flds f = (.::) (ssl (pname ++ " {") : fields f ++ [scl '}'])

        fields [] = []
        fields fs = scl ' ' : concat (intersperse [ssl ", "] (map field (zip [0..] fs))) ++ [scl ' ']

        field (n,f) = [ ssl (f ++ " = ") , l2 "showsPrec" (lit (0::Integer)) (vrn 'x' n) ]

        ssl = l1 "showString" . lit ; scl = l1 "showChar" . lit

        pname = show (ppr (mkName (ctorName ctr)))
