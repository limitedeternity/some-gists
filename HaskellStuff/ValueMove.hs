{- 
    Using TypeLits + case pattern-matching to move value to and from type level 
-}

{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables #-}

import GHC.TypeLits
import Data.Typeable

data Box (n :: Nat) = Box

box :: KnownNat n => Box n -> Integer
box b@Box = natVal b

medium n = case someNatVal n of
             Just (SomeNat (_ :: Proxy x)) -> do
                 let b :: Box x
                     b = Box
                 return $ box b
