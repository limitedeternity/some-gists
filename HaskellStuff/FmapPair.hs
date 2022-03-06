{-
    λ> fmap (+1) [1,2,3]
    [2,3,4]
    λ> fmap (+1) (1,2)
    (1,3)
-}

newtype T2 a = T2 { asPair :: (a, a) }
    deriving (Show, Eq)

instance Functor T2 where
    fmap f (T2 (x, y)) = T2 (f x, f y)

fmap_each :: (a -> b) -> (a, a) -> (b, b)
fmap_each f p = asPair $ fmap f (T2 p)

{-
    λ> fmap_each (+1) (1,2)
    (2,3)
-}
