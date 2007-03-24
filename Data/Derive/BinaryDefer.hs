
module Data.Derive.BinaryDefer(derive) where

import Data.Derive
import Data.List

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
