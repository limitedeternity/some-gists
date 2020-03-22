{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.List (intercalate)

-- Приоритет | Операция | Определение | Обозначение |
-- ------------------------------------------------- 
-- 10        |    NOT   |    not x    |      -|      
-- 5         |    AND   |    x |*| y  |      Λ       
-- 4         |    OR    |    x |+| y  |      V       
-- 4         |    XOR   |    x |^| y  |     (+), ^   
-- 3         |    NAND  |    x ↑ y    |      |, ↑    
-- 2         |    NOR   |    x ↓ y    |      ↓       
-- 1         |    IMP   |    x ~> y   |      ->      
-- 0         |    EQ    |    x <-> y  |   <->, ==    
-- -------------------------------------------------|
--
-- Операторы левоассоциативны
--


(<->) :: Bool -> Bool -> Bool
(<->) = (==)
infixl 0 <->

(~>) :: Bool -> Bool -> Bool
(~>) x y = not x || y
infixl 1 ~>

(↓) :: Bool -> Bool -> Bool
(↓) x y = not (x || y)
infixl 2 ↓

(↑) :: Bool -> Bool -> Bool
(↑) x y = not (x && y)
infixl 3 ↑

(|^|) :: Bool -> Bool -> Bool
(|^|) = (/=)
infixl 4 |^|

(|+|) :: Bool -> Bool -> Bool
(|+|) = (||)
infixl 4 |+|

(|*|) :: Bool -> Bool -> Bool
(|*|) = (&&)
infixl 5 |*|

-- Forward pipe operator
(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>

class CallWithList f res where
    t :: f -> [res] -> res

instance CallWithList res res where
    t res [] = res
    t _ _ = error "слишком много аргументов"

instance CallWithList f res => CallWithList (res -> f) res where
    t f (x:xs) = t (f x) xs
    t _ [] = error "недостаточно аргументов"


cartSelfProd :: (Monad m, Num a, Enum a) => m b -> a -> m [b]
cartSelfProd l n = mapM (\_ -> l) [1..n]


buildTable :: (CallWithList f Bool, Num a, Enum a) => a -> f -> [([Bool], Bool)]
buildTable argsAmount fn =
    (\c -> zip c $ map (t fn) c) (cartSelfProd [False, True] argsAmount)


main :: IO ()
main =
    buildTable 3 fn
    |> map show
    |> intercalate "\n"
    |> putStrLn
    where
        fn = \x -> \y -> \z -> (x |*| y ~> z) |+| x |^| y

