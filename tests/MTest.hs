{-# LANGUAGE CPP  #-}
module MTest where
import M1
import qualified M2
import Data.DeriveTH
import Data.Generics
#ifndef GHC_DERIVING
import M1I
import M2I
#endif

m1f :: Field
m1f = F3 { f2 = One (Id 4), f1 = F0, f3 = Two (Id2 False) Null }
m2f :: M2.Field 
m2f = M2.F3 { M2.f2 = M2.One (M2.Id 4), M2.f1 = M2.F0, M2.f3 = M2.Two (M2.Id2 False) M2.Null }

gen1 = (gsize m1f, gsize m2f)
gen2 = (everything (+) (gtypecount (undefined :: Field )) (m1f,m1f), 
        everything (+) (gtypecount (undefined :: Field )) (m2f,m2f), 
        everything (+) (gtypecount (undefined :: M2.Field )) (m2f,m2f), 
        everything (+) (gtypecount (undefined :: Field )) (m1f,m2f),
        everything (+) (gtypecount (undefined :: Id Int)) (m1f,m2f),        
        everything (+) (gtypecount (undefined :: M2.Id2 Bool)) (m2f,m2f)
        ) 
fields :: (Data a) => a -> [String]
fields = fieldsOfDataRep. dataTypeRep . dataTypeOf where 
    fieldsOfDataRep (AlgRep constr)= concatMap constrFields constr
    fieldsOfDataRep _ = []
    
main = do
    print (typeOf m1f)
    print (typeOf m2f)
    print (dataTypeOf m1f)
    print (dataTypeOf m2f)
    print gen1 
    print gen2
    print (fields m1f)
    print (fields m2f)
    print (fields (Id2 False))
    print (fields (M2.Id2 True))
    