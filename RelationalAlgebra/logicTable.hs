{-
   Программа для построения и минификации таблицы истинности функций от двух или трёх переменных
   Автор: Беспалов В. (3 курс, ИС)
   Окружение, использованное при разработке: GHC (8.10.x)
-}

{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TupleSections #-}

module LogicTable where
import Data.List (intercalate, elemIndex, nub)
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

instance {-# OVERLAPPING #-} Arity f => Arity ((->) a f) where
    arity f = 1 + arity (f undefined)


cartSelfProd :: (Monad m, Num a, Enum a) => m b -> a -> m [b]
cartSelfProd l n = mapM (const l) [1..n]


numToBool :: (Eq a, Num a) => a -> Bool
numToBool = (==1)

buildFnFromVec4 :: [Bool] -> (Bool -> Bool -> Bool)
buildFnFromVec4 lst
    | length lst == 4 =
        \x y -> (!!) lst $ fromJust $ elemIndex [x, y] $ cartSelfProd [False, True] 2
    | otherwise = error "вектор некорректной длины"

buildFnFromVec8 :: [Bool] -> (Bool -> Bool -> Bool -> Bool)
buildFnFromVec8 lst
    | length lst == 8 =
        \x y z -> (!!) lst $ fromJust $ elemIndex [x, y, z] $ cartSelfProd [False, True] 3
    | otherwise = error "вектор некорректной длины"


buildTable :: (CallWithList f Bool, Arity f) => f -> [([Bool], Bool)]
buildTable fn =
    (\c -> zip c $ map (callwithlist fn) c) (cartSelfProd [False, True] $ arity fn)


checkFictivity :: [([Bool], Bool)] -> [Bool]
checkFictivity table
    | length table == 4 =
        map isFictive 
            [
                [(i, i+2) | i <- [0, 1]], -- x
                [(i, i+1) | i <- [0, 2]]  -- y
            ]
    | length table == 8 =
        map isFictive
            [
                [(i, i+4) | i <- [0, 1, 2, 3]], -- x
                [(i, i+2) | i <- [0, 1, 4, 5]], -- y
                [(i, i+1) | i <- [0, 2, 4, 6]]  -- z
            ]
    | otherwise = error "таблица неподдерживаемой ширины"
        where
            isFictive = all (\pair -> (==) (snd $ (!!) table (fst pair)) (snd $ (!!) table (snd pair)))


minifyTable :: [([Bool], Bool)] -> [Bool] -> [([Bool], Bool)]
minifyTable table fictivityVector
    | 2 ^ length fictivityVector == length table = nub $ filterOutFictiveVars $ getIndexesOfTrue fictivityVector
    | otherwise = error "некорректные данные"
        where
            getIndexesOfTrue :: (Num b, Enum b) => [Bool] -> [b]
            getIndexesOfTrue lst = map snd $ filter fst $ zip lst [0..]

            filterOutFictiveVars :: (Num b, Enum b, Eq b) => [b] -> [([Bool], Bool)]
            filterOutFictiveVars fictiveVarIndexes = map (
                \pair -> 
                    (,snd pair) (
                        map fst $ flip filter (zip (fst pair) [0..]) $ \enumeratedVarState ->
                            snd enumeratedVarState `notElem` fictiveVarIndexes 
                    )
                ) table


main :: IO ()
main = do
    let table = buildTable fn
    putStrLn "Таблица истинности:"
    putStrLn $ intercalate "\n" $ map show table
    putStrLn ""

    let fictivityVector = checkFictivity table
    putStrLn "Фиктивность аргументов:"
    putStrLn $ unwords $ map show fictivityVector
    putStrLn ""

    let minTable = minifyTable table fictivityVector
    putStrLn "Минифицированная таблица:"
    putStrLn $ intercalate "\n" $ map show minTable
    putStrLn ""

    where
        {-
            Примеры использования:
            1) Лямбда-выражение:
                fn = \x y z -> not (x |+| y) ~> not (z |*| y)
            2) Функции:
                fn x y z = not (x |+| y) ~> not (z |*| y)
            3) Результирующий вектор (двух переменных)
                fn = buildFnFromVec4 [False, True, True, False]
            4) Результирующий вектор (трёх переменных)
                fn = buildFnFromVec8 [False, False, True, True, True, True, False, False]

            Затем, либо выполнить компиляцию и запустить, либо загрузить этот файл в GHCi и вызвать main.
        -}

        -- Функция (не менять имя):
        fn = \x y z -> not (x |+| y) ~> not (z |*| y)

