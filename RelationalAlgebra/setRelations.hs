{-
   Программа для поиска отношений между функциями на множествах (от двух или трёх переменных).
   Автор: Беспалов В. (3 курс, ИС)
   Окружение, использованное при разработке: GHC (8.10.x)

   Полезные материалы:
   * Л. Ю. Белова, Ю. А. Белов: "Элементы теории множеств и математической логики"
        http://www.lib.uniyar.ac.ru/edocs/iuni/20120210.pdf
-}

{-# LANGUAGE FlexibleInstances #-}

module SetRelations where
import LogicTable hiding (main)
import Control.Monad (forM_)
import qualified Data.Bifunctor as Bifunctor (first)
import Data.List (intercalate)


class AmbiguousOperators t where
    (⊂) :: t -> t -> Bool
    (≡) :: t -> t -> Bool
    (⊆) :: t -> t -> Bool


-- Операторы для "выяснения отношений" для таблиц истинности
instance AmbiguousOperators [([Bool], Bool)] where
    (⊂) a b = all (uncurry (~>)) $ zip (map snd a) (map snd b)
    (≡) a b = all (uncurry (<->)) $ zip (map snd a) (map snd b)
    (⊆) a b = all (uncurry (~>)) $ zip (map snd a) (map snd b)

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
exclCartSelfProd2 lst = [(\v -> (,) (snd $ fst v) (snd $ snd v)) (x, y) | x <- zip [0..] lst, y <- zip [0..] lst, fst x /= fst y]


main :: IO ()
main = do
    let fnTables = map buildTable fnList

    forM_ (zip fnTables fnNameList) $ \tableAndName -> 
        putStrLn ("Таблица истинности для функции " ++ snd tableAndName ++ ":") >> 
            putStrLn (intercalate "\n" $ map show $ fst tableAndName) >> 
                putStrLn ""

    let fnTableComb = exclCartSelfProd2 fnTables
    let fnNameComb = exclCartSelfProd2 fnNameList
    let inclusions = filter fst $ flip map (zip fnTableComb fnNameComb) $ \tablesAndNames -> Bifunctor.first (uncurry (⊂)) tablesAndNames

    putStrLn "Найденные отношения между множествами:"
    forM_ inclusions $ \incl -> 
            if (True, (snd $ snd incl, fst $ snd incl)) `elem` inclusions then putStrLn $ fst (snd incl) ++ " == " ++ snd (snd incl)
            else putStrLn $ fst (snd incl) ++ " ⊂ " ++ snd (snd incl)

    where
        {-
            Примеры использования:
            1) Лямбда-выражение:
                d = \a b c -> not (a ∪ c) ∪ (b ∩ c) ∪ (not b ∩ not c)
                e = \a b c -> not (b △ c) ∪ (b \\ a)
                f = \a b c -> b ∪ not c
            2) Функции:
                d a b c = not (a ∪ c) ∪ (b ∩ c) ∪ (not b ∩ not c)
                e a b c = not (b △ c) ∪ (b \\ a)
                f a b c = b ∪ not c

            Затем, либо выполнить компиляцию и запустить, либо загрузить этот файл в GHCi и вызвать main.
        -}

        -- Функции:
        d a b c = not (a ∪ c) ∪ (b ∩ c) ∪ (not b ∩ not c)
        e a b c = not (b △ c) ∪ (b \\ a)
        f a b c = b ∪ not c

        {-
           Дополнительные "переменные".
           * fnList – список анализируемых функций; 
                их может быть сколько угодно, поэтому программа ссылается не на конкретные функции, а на их список.
           * fnNameList – список с названиями анализируемых функций;
                используется для обозначений при выводе на экран.
        -}

        -- Дополнительные "переменные":
        fnList = [d, e, f]
        fnNameList = ["d", "e", "f"]

