{-# LANGUAGE MultiWayIf #-}

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
zhegalkinReduce table = [snd x | x <- (zip [0..] table), (fst x) `elem` (map (fst) $ filter (snd) $ zip [0..] $ map (!!0) $ buildXorPyramid $ map (snd) $ table)]
    where
        buildXorPyramid vec | (>=) (length vec) 2 = vec:(xorDescend vec)
                            | otherwise = vec:[]
            where
                xorDescend v = (map (uncurry (|^|)) $ zip v $ tail v) & \row -> 
                    if | (>) (length row) 0 -> row:(xorDescend row)
                       | otherwise -> []


main :: IO ()
main = do
    let table = buildTable fn
    putStrLn "Таблица истинности:"
    putStrLn $ intercalate "\n" $ map show $ table
    putStrLn ""

    let zhegalkinTable = zhegalkinReduce table
    putStrLn "Полином Жегалкина:"
    putStrLn $ intercalate " |^| " $ map (intercalate " |*| ") $ (\conj -> (>) (length conj) 0 ? conj :? [["False"]]) $ (flip map) (map (filter (snd)) $ map (zip [0..]) $ map (fst) $ zhegalkinTable) $ \trueVarsPairs -> 
        if | (>) (length trueVarsPairs) 0 -> map (\p -> (!!) (varNames) (fst p)) trueVarsPairs
           | otherwise -> ["True"]
    putStrLn ""

    putStrLn "СДНФ:"
    putStrLn $ (\s -> "(" ++ s ++ ")") $ intercalate ") |+| (" $ map (intercalate " |*| ") $ (flip map) (map (zip [0..]) $ map (fst) $ filter (snd) $ table) $ map $ \p -> 
        if | (snd p == True) -> (!!) (varNames) (fst p)
           | otherwise -> (++) "!" $ (!!) (varNames) (fst p)
    putStrLn ""
    
    putStrLn "СКНФ:"
    putStrLn $ (\s -> "(" ++ s ++ ")") $ intercalate ") |*| (" $ map (intercalate " |+| ") $ (flip map) (map (zip [0..]) $ map (fst) $ filter (not . snd) $ table) $ map $ \p -> 
        if | (snd p == False) -> (!!) (varNames) (fst p)
           | otherwise -> (++) "!" $ (!!) (varNames) (fst p)
    putStrLn ""

    where
        -- Функцию заносить сюда:
        fn = buildFnFromVec8 [True, False, True, True, True, True, False, False]
        -- Дополнительные переменные:
        varNames = ["x", "y", "z"]

