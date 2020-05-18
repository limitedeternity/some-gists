{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}

module SetRelations where
import LogicTable hiding (main)
import Data.List (intercalate)


class AmbiguousOperators t where
    (⊂) :: t -> t -> Bool
    (≡) :: t -> t -> Bool
    (⊆) :: t -> t -> Bool


-- Операторы для "выяснения отношений" для таблиц истинности
instance AmbiguousOperators [([Bool], Bool)] where
    (⊂) a b = all (id) $ map (uncurry (~>)) $ zip (map (snd) a) (map (snd) b)
    (≡) a b = all (id) $ map (uncurry (<->)) $ zip (map (snd) a) (map (snd) b)
    (⊆) a b = all (id) $ map (uncurry (~>)) $ zip (map (snd) a) (map (snd) b)

-- Операторы для "выяснения отношений" для булевых функций
instance AmbiguousOperators Bool where
    (⊂) = (~>)
    (≡) = (<->)
    (⊆) = (~>)

-- Определения приоритетов, не влезшие в тайп-классы
infixl 0 ⊂
infixl 0 ≡
infixl 0 ⊆

-- Операторы для перегона "сетовых" выражений в обычные булевы ф-ции
(∩) :: Bool -> Bool -> Bool
(∩) = (|*|)
infixl 0 ∩

(×) :: Bool -> Bool -> Bool
(×) = (|*|)
infixl 0 ×

(∪) :: Bool -> Bool -> Bool
(∪) = (|+|)
infixl 0 ∪

(\\) :: Bool -> Bool -> Bool
(\\) a b = not (a ~> b)
infixl 0 \\

(△) :: Bool -> Bool -> Bool 
(△) a b = a |*| not b |+| not a |*| b
infixl 0 △


exclCartSelfProd2 :: [a] -> [(a, a)]
exclCartSelfProd2 lst = map (\v -> (,) (snd $ fst v) (snd $ snd v)) [(x, y) | x <- (zip [0..] lst), y <- (zip [0..] lst), fst x /= fst y]


main :: IO ()
main = do
    let fnTables = map buildTable fnList

    (flip mapM_) (zip fnTables fnNameList) $ \tableAndName -> 
        (putStrLn $ "Таблица истинности для функции " ++ (snd tableAndName) ++ ":") >> 
            (putStrLn $ intercalate "\n" $ map show $ fst tableAndName) >> 
                (putStrLn "")

    let fnTableComb = exclCartSelfProd2 fnTables
    let fnNameComb = exclCartSelfProd2 fnNameList
    let inclusions = filter (fst) $ (flip map) (zip fnTableComb fnNameComb) $ \tablesAndNames ->
            if | (fst $ fst tablesAndNames) ⊂ (snd $ fst tablesAndNames) -> (True, (fst $ snd tablesAndNames, snd $ snd tablesAndNames))
               | otherwise -> (False, (fst $ snd tablesAndNames, snd $ snd tablesAndNames))

    putStrLn "Найденные отношения между множествами:"
    (flip mapM_) inclusions $ \incl -> 
            if | (True, (snd $ snd incl, fst $ snd incl)) `elem` inclusions -> putStrLn $ (fst $ snd incl) ++ " == " ++ (snd $ snd incl)
               | otherwise -> putStrLn $ (fst $ snd incl) ++ " ⊂ " ++ (snd $ snd incl)

    where
        -- Функции заносить сюда:
        d a b c = not (a ∪ c) ∪ (b ∩ c) ∪ (not b ∩ not c)
        e a b c = not (b △ c) ∪ (b \\ a)
        f a b c = b ∪ not c
        -- Дополнительные переменные:
        fnList = [d, e, f]
        fnNameList = ["d", "e", "f"]

