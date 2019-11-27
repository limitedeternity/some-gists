{-# LANGUAGE BangPatterns #-}
module Power (power) where

power _ 0 = 1
power 0 _ = 0
power x n  = let sqr k = k * k
                 half_n = n `div` 2
                 sqrHalfPower = sqr ( power x half_n )
             in if even n then sqrHalfPower else x * sqrHalfPower

