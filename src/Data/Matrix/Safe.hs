{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : Data.Matrix.Safe
Description : Type safe matrix wrapper over the matrix library
Copyright   : (c) Wanja Chresta, 2018
License     : GPL-3
Maintainer  : wanja.hs@chrummibei.ch
Stability   : experimental
Portability : POSIX

Naive implementation of coding theory linear codes and error correcting codes
over arbitrary fields, including finite fields. Goes well with the
@HaskellForMath@ library and its finite field implementations in
@Math.Algebra.Field@. To use extension fields (fields of prime power, i.e.
@F_{p^k}@ with @k>1@), use one of the exported finite fields in
@Math.Algebra.Field.Extension@ like 'F16' and its generator 'a16'.
-}

module Data.Matrix.Safe
    ( Matrix(..)
    , matrix
    , Vector
    , transpose
    , (<|>)
    , identity
    , zero
    , fromList
    , fromLists
    , (.*)
    , rref
    ) where

import GHC.TypeLits (Nat, KnownNat, natVal, type (+), type (-), type (<=))
import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (mappend)

import qualified Data.Matrix as M

newtype Matrix (m :: Nat) (n :: Nat) (f :: *) = Matrix (M.Matrix f)
    deriving (Functor, Applicative, Foldable, Eq, Monoid)

instance forall m n f. Show f => Show (Matrix m n f) where
    show (Matrix mat) = M.prettyMatrix mat

instance forall f m n. Num f => Num (Matrix m n f) where
    (Matrix x) + (Matrix y) = Matrix $ x + y
    (Matrix x) - (Matrix y) = Matrix $ x - y
    (*) = error "Data.Matrix.Safe: (*) not allowed. Use (.*) instead"
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = Matrix . fromInteger

-- | Type safe matrix multiplication
(.*) :: forall m k n a. Num a => Matrix m k a -> Matrix k n a -> Matrix m n a
(Matrix m) .* (Matrix n) = Matrix $ m * n

type Vector = Matrix 1

matrix :: forall m n a. (KnownNat m, KnownNat n)
       => ((Int, Int) -> a) -> Matrix (m :: Nat) (n :: Nat) a
matrix = Matrix . M.matrix m' n'
    where m' = fromInteger $ natVal (Proxy :: Proxy m)
          n' = fromInteger $ natVal (Proxy :: Proxy n)

transpose :: forall m n a. Matrix m n a -> Matrix n m a
transpose (Matrix m) = Matrix . M.transpose $ m

(<|>) :: forall m n k a. (KnownNat n, KnownNat k)
      => Matrix m n a -> Matrix m k a -> Matrix m (k+n) a
(Matrix x) <|> (Matrix y) = Matrix $ x M.<|> y

identity :: forall n a. (Num a, KnownNat n) => Matrix n n a
identity = Matrix $ M.identity n'
    where n' = fromInteger $ natVal (Proxy :: Proxy n)

zero :: forall m n a. (Num a, KnownNat n, KnownNat m) => Matrix m n a
zero = Matrix $ M.zero m' n'
    where n' = fromInteger $ natVal (Proxy :: Proxy n)
          m' = fromInteger $ natVal (Proxy :: Proxy m)

fromList :: forall m n a. (KnownNat m, KnownNat n) => [a] -> Matrix m n a
fromList as = if length as == n'*m' 
                 then Matrix $ M.fromList m' n' as
                 else error $ "List has wrong dimension: "
                                <>show (length as)
                                <>" instead of "
                                <>show (n'*m')
    where n' = fromInteger $ natVal (Proxy :: Proxy n)
          m' = fromInteger $ natVal (Proxy :: Proxy m)

fromLists :: forall m n a. (KnownNat m, KnownNat n) => [[a]] -> Matrix m n a
fromLists as = if length as == m' && length (head as) == n'
                 then Matrix $ M.fromLists as
                 else error $ "List has wrong dimension: "
                                <>show (length as)<>":"
                                <>show (length $ head as)
                                <>" instead of "
                                <>show m' <>":"<> show n'
    where n' = fromInteger $ natVal (Proxy :: Proxy n)
          m' = fromInteger $ natVal (Proxy :: Proxy m)


rref :: forall m n a. (Fractional a, Eq a, KnownNat m, KnownNat n, m <= n) 
     => Matrix m n a -> Either String (Matrix m n a)
rref (Matrix m) = Matrix <$> M.rref m


