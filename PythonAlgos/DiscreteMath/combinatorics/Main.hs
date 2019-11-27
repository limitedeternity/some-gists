import Factorial

a :: Int -> Int -> Double
a k n = (fromInteger $ factorial n) / (fromInteger $ factorial (n - k))

a_ :: Int -> Int -> Double
a_ k n = (fromIntegral $ n) ** (fromIntegral $ k)

p :: Int -> Double
p n = fromInteger $ factorial n

p_ :: Int -> [Int] -> Double
p_ n lst = (fromInteger $ factorial n) / (foldl (*) 1 (map (fromInteger . factorial) lst)) 

c :: Int -> Int -> Double
c k n = (fromInteger $ factorial n) / ((fromInteger $ factorial k) * (fromInteger $ factorial (n - k)))

c_ :: Int -> Int -> Double
c_ k n = (fromInteger $ factorial (n + k - 1)) / ((fromInteger $ factorial k) * (fromInteger $ factorial (n - 1)))

