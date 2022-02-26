{-
    Бустрофедоновое произведение:
    boxall [[1,2,3], [4,5,6]] == [4,5,6,1,6,5,4,2,4,5,6,3,6,5,4]
-}

xs ☐ ys = mix xs (ys, reverse ys)
    where
        mix [] (ys, sy) = ys
        mix (x : xs) (ys, sy) = ys ++ [x] ++ mix xs (sy, ys)

boxall :: [[a]] -> [a]
boxall = foldr (☐) []
