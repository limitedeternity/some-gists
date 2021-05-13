{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-}

class DeepReverse l where 
    deepReverse :: l -> l

instance {-# OVERLAPPING #-} DeepReverse [a] => DeepReverse [[a]] where
    deepReverse = map deepReverse

instance DeepReverse [a] where
    deepReverse = reverse


main :: IO()
main = do
    print $ deepReverse [[[[1 :: Int, 2, 3], [4, 5, 6]]]]
    print $ deepReverse [[[1 :: Int, 2, 3], [4, 5, 6]]]
    print $ deepReverse [[1 :: Int, 2, 3], [4, 5, 6]]
    print $ deepReverse [1 :: Int, 2, 3]
