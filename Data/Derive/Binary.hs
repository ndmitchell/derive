
module Data.Derive.Binary(derive) where

import Data.Derive
import Data.List

derive :: DataDef -> [String]
derive dat@(DataDef name arity ctors) =
        instanceHead "Binary" dat :
        put items ++ get items
    where
        nctors = length ctors
        bitWidth = if nctors <= 256 then "8"
                   else if nctors <= 65536 then "16"
                   else "32"
        items = zip [0..] ctors
        
        
        put [(_, ctor)] = [put2 "" ctor]
        put xs = [put2 ("putWord" ++ bitWidth ++ " " ++ show a) b | (a,b) <- xs]
        
        put2 prefix (CtorDef name arity)
            | arity == 0 = "    put " ++ name ++ " = " ++ (if null prefix then "return ()" else prefix)
            | otherwise  = "    put (" ++ name ++ concatMap (' ':) typ ++ ") = " ++ prefix ++
                          concatMap (" >> put " ++) typ
                where typ = map (:[]) $ take arity ['a'..]

        get [(_, ctor)] = ["    get = " ++ get2 ctor]
        get xs = "    get = do" :
                 ("        tag_ <- getWord" ++ bitWidth) :
                 "        case tag_ of" :
                 ["            " ++ show a ++ " -> " ++ get2 b | (a,b) <- xs]

        get2 (CtorDef name arity)
            | arity == 0 = "return " ++ name
            | otherwise  = concatMap (\x -> "get >>= \\" ++ x ++ " -> ") typ ++
                           "return (" ++ name ++ concatMap (' ':) typ ++ ")"
                where typ = map (:[]) $ take arity ['a'..]
