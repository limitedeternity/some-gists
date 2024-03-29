{-
   Программа для построения полинома Жегалкина по функции от двух или трёх переменных.
   Автор: Беспалов В. (3 курс, ИС)
   Окружение, использованное при разработке: GHC (8.10.x)
-}

module ZhegalkinPoly where
import Data.Function ((&))
import Data.List (intercalate)
import LogicTable hiding (main)


data Cond a = a :? a
infixl 0 ?
infixl 1 :?

(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y


zhegalkinReduce :: [([Bool], Bool)] -> [([Bool], Bool)]
zhegalkinReduce table = [snd x | x <- zip [0..] table, fst x `elem` map fst (filter snd $ zip [0..] $ map (!!0) $ buildXorPyramid $ map snd table)]
    where
        buildXorPyramid vec | length vec >= 2 = vec:xorDescend vec
                            | otherwise = [vec]
            where
                xorDescend v = zipWith (|^|) v (tail v) & \row -> 
                    if not (null row) then row:xorDescend row
                    else []


main :: IO ()
main = do
    let table = buildTable fn
    putStrLn "Таблица истинности:"
    putStrLn $ intercalate "\n" $ map show table
    putStrLn ""

    let zhegalkinTable = zhegalkinReduce table
    putStrLn "Полином Жегалкина:"
    putStrLn $ intercalate " |^| " $ map (intercalate " |*| ") $ (\conj -> (>) (length conj) 0 ? conj :? [["False"]]) $ flip map (map ((filter snd . zip [0..]) . fst) zhegalkinTable) $ \trueVarsPairs -> 
        if not (null trueVarsPairs) then map ((!!) varNames . fst) trueVarsPairs
        else ["True"]

    putStrLn ""

    putStrLn "СДНФ:"
    putStrLn $ (\s -> "(" ++ s ++ ")") $ intercalate ") |+| (" $ map (intercalate " |*| ") $ flip map (map (zip [0..] . fst) (filter snd table)) $ map $ \p -> 
        if snd p then (!!) varNames (fst p)
        else (++) "!" $ (!!) varNames (fst p)

    putStrLn ""
    
    putStrLn "СКНФ:"
    putStrLn $ (\s -> "(" ++ s ++ ")") $ intercalate ") |*| (" $ map (intercalate " |+| ") $ flip map (map (zip [0..] . fst) (filter (not . snd) table)) $ map $ \p -> 
        if snd p then (++) "!" $ (!!) varNames (fst p)
        else (!!) varNames (fst p)

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
        fn = buildFnFromVec8 [False, False, True, True, True, True, False, False]

        {-
           Дополнительные "переменные".
           * varNames – список с названиями переменных функции;
                используется при построении полинома и выводе на экран.
        -}

        -- Дополнительные "переменные":
        varNames = ["x", "y", "z"]

