{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, ScopedTypeVariables,
    GeneralizedNewtypeDeriving #-}
module Numeric.Field.Finite
    ( Field, characteristic
    , GFp, gfp, GF(..), GF2
    ) where

import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Modular (Mod(..), type ℤ, type (/)(), toMod)
import Data.Proxy (Proxy (..))
import Data.Bifunctor (first)

-- |Galois field of prime order GF(p) where p is prime
newtype GF (p :: Nat) (s :: Nat) = GF (ℤ/p) deriving (Eq, Num)
instance (KnownNat p, KnownNat s) => Bounded (GF p s) where
    minBound = GF . toMod $ 0
    maxBound = GF . toMod $ p'^s'-1
        where p' = fromInteger $ natVal (Proxy :: Proxy p)
              s' = fromInteger $ natVal (Proxy :: Proxy s)

class (Num f, Eq f, Monoid f) => Field f where
    characteristic :: Proxy f -> Integer
instance KnownNat p => Field (GF p s) where
    characteristic _ = fromInteger $ natVal (Proxy :: Proxy p)

--newtype GFp (p :: Nat) = GFp (ℤ/p) deriving (Eq, Num)
type GFp (p :: Nat) = GF p 1
type GF2 = GFp 2

instance Show (GF p s) where
    show (GF x) = show x
instance KnownNat p => Monoid (GF p s) where
    mappend = (+)
    mempty = 0

gfp :: forall p. KnownNat p => Integer -> GFp p
gfp = GF . toMod

class FiniteField f
instance FiniteField (GF p s)

