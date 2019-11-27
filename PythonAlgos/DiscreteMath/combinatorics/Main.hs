import Factorial
import Power

a :: Int -> Int -> Double
a k n = (fromInteger $ factorial n) / (fromInteger $ factorial (n - k))

a_ k n = power n k
p n = factorial n

c :: Int -> Int -> Double
c k n = (fromInteger $ factorial n) / ((fromInteger $ factorial k) * (fromInteger $ factorial (n - k)))

c_ :: Int -> Int -> Double
c_ k n = (fromInteger $ factorial (n + k - 1)) / ((fromInteger $ factorial k) * (fromInteger $ factorial (n - 1)))

