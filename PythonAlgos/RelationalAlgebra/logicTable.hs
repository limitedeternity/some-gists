{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.List (intercalate, elemIndex)
import Data.Maybe (fromJust)

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
    callwithlist :: f -> [res] -> res

instance CallWithList res res where
    callwithlist res [] = res
    callwithlist _ _ = error "слишком много аргументов"

instance CallWithList f res => CallWithList (res -> f) res where
    callwithlist f (x:xs) = callwithlist (f x) xs
    callwithlist _ [] = error "недостаточно аргументов"


class Arity f where
    arity :: f -> Int

instance Arity x where
    arity _ = 0

instance Arity f => Arity ((->) a f) where
    arity f = 1 + arity (f undefined)


cartSelfProd :: (Monad m, Num a, Enum a) => m b -> a -> m [b]
cartSelfProd l n = mapM (\_ -> l) [1..n]


buildFnFromVec4 :: [Bool] -> (Bool -> Bool -> Bool)
buildFnFromVec4 lst
    | (length lst == 4) =
        \x -> \y -> (!!) lst $ fromJust $ elemIndex [x, y] $ cartSelfProd [False, True] 2
    | otherwise = error "вектор некорректной длины"

buildFnFromVec8 :: [Bool] -> (Bool -> Bool -> Bool -> Bool)
buildFnFromVec8 lst
    | (length lst == 8) =
        \x -> \y -> \z -> (!!) lst $ fromJust $ elemIndex [x, y, z] $ cartSelfProd [False, True] 3
    | otherwise = error "вектор некорректной длины"


buildTable :: (CallWithList f Bool, Num a, Enum a) => a -> f -> [([Bool], Bool)]
buildTable argsAmount fn =
    (\c -> zip c $ map (callwithlist fn) c) (cartSelfProd [False, True] argsAmount)


main :: IO ()
main =
    buildTable (arity fn) fn
    |> map show
    |> intercalate "\n"
    |> putStrLn
    where
        fn x1 x2 x3 = f (g x1 x2) x1 |*| g x1 x3
            where
                f = buildFnFromVec4 [False, True, True, True]
                g = buildFnFromVec4 [True, True, False, True]

