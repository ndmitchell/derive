
module Data.Derive.BinaryDefer(binarydefer) where

import Data.Derive
import Data.List

binarydefer :: Derivation
binarydefer = Derivation derive

derive :: DataDef -> [String]
derive dat@(DataDef name arity ctors) =
        instanceHead "BinaryDefer" dat :
        "    bothDefer = defer" :
        ("        [" ++ r) :
        map ("        ," ++) es ++
        ["        ]"]
    where
        r:es = map f ctors
        
        f (CtorDef name arity _)= "\\ ~(" ++ name ++ concatMap (' ':) typs ++ ") -> unit " ++
                                  name ++ concatMap (" << " ++) typs
            where typs = map (:[]) $ take arity ['a'..]
