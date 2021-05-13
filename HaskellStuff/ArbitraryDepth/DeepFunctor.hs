{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

class DeepFunctor a b fga where
    dmap :: (a -> b) -> fga -> Fmapped b fga

instance (Fmapped b a ~ b) => DeepFunctor a b a where
    dmap = id

instance (DeepFunctor a b ga, Functor f) => DeepFunctor a b (f ga) where
    dmap = fmap . dmap

type family Fmapped b fga where
    Fmapped b (f ga) = f (Fmapped b ga)
    Fmapped b a = b


main :: IO()
main = do
    print $ dmap ((+1) :: Int -> Int) [[[1 :: Int, 2, 3], [3, 4, 5]], [[2, 4, 6], [3, 5, 7]]]
    print $ dmap (replicate 3 :: Int -> [Int]) (Just (Just (Just (Just (0 :: Int)))))
