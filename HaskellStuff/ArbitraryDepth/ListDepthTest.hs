{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-}

import Data.Typeable
import Data.List

-- Первый способ: полиморфизм
class Depth l where
    listDepth :: l -> Integer

instance {-# OVERLAPPING #-} Depth l => Depth [l] where
    listDepth = (+1) . foldr (\x y -> if x > y then x else y) 0 . map listDepth 

instance Depth l where
    listDepth = const 0


-- Второй способ: запрос типа у компилятора
listDepth' :: Typeable t => t -> Integer
listDepth' = typeBrackets . decltype
    where
        decltype :: Typeable t => t -> String
        decltype = show . typeOf

        typeBrackets :: String -> Integer
        typeBrackets lt | isPrefixOf "[" lt = toInteger . length $ takeWhile (=='[') lt
                        | otherwise = error "argument is not a list type"
