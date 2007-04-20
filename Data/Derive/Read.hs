
-- | Derives @Read@.  This is as defined by the Haskell report, except
-- there is no support for infix constructors.  If you attempt to
-- derive @Read@ for a data type with infix constructors, the
-- constructors are handled as if they were prefix constructors, using
-- the @(@/consym/@)@ syntax.
module Data.Derive.Read(makeRead) where

import Language.Haskell.TH.All
import Data.List
import Data.Char

makeRead :: Derivation
makeRead = Derivation read' "Read"

read' dat = [instance_default "Read" dat [funN "readsPrec" [sclause [vr "p0", vr "r0"] body]]]
    where
        body = (++::) [ readit ctr | ctr <- dataCtors dat ]

readit ctr = case ctorFields ctr of [] -> norm
                                    fl -> flds fl
    where
        norm = l2 "readParen" (vr "p0" >: lit (10::Integer))
               (runComp (pName . foldr (.) id (map (pRead 11) (ctv ctr 'x'))) (ctp ctr 'x'))

        flds f = l2 "readParen" false
                 (runComp (pName . pLex "{" .
                           foldr (.) id (intersperse (pLex ",")
                                         (zipWith pField (ctv ctr 'x') f)) .
                           pLex "}") (ctp ctr 'x'))

        runComp fn ex = CompE $ fn (\k -> [ NoBindS (tup [ex, vrn 'r' k]) ]) 0

        pArse pat fun ct k = BindS (tup [pat, vrn 'r' (k+1)]) (AppE fun (vrn 'r' k)) : ct (k+1)

        pLex pat = pArse (lit pat) (l0 "lex")

        name = ctorName ctr

        pName | isAlpha (head name) || head name == '_' = pLex name
              | otherwise                               = pLex "(" . pLex name . pLex ")"

        pRead pc pat = pArse pat (l1 "readsPrec" (lit (pc :: Integer)))

        pField pat fld = pLex fld . pLex "=" . pRead 0 pat
