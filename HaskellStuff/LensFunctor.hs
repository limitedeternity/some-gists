{-# LANGUAGE TypeApplications #-} 
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE AllowAmbiguousTypes #-} 
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE UndecidableInstances #-} 

import GHC.TypeLits

class LensFunctor (n :: Nat) a b s t where
    lmap :: (a -> b) -> (s -> t)

instance {-# OVERLAPPING #-} (a ~ a', b ~ b') => LensFunctor 0 a b a' b' where
    lmap = id

instance {-# INCOHERENT #-} (s ~ f s', t ~ f t', LensFunctor (n - 1) a b s' t', Functor f) => LensFunctor n a b s t where
    lmap = fmap . lmap @(n - 1)


main :: IO()
main = do
    print $ lmap @2 reverse [[[1,2,3], [3,4,5]], [[2,4,6], [3,5,7]]]
    print $ lmap @4 (replicate 3) (Just (Just (Just (Just 0))))

