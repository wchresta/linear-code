{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-
    This file is part of linear-codes.

    Linear-Codes is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Foobar is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Foobar.  If not, see <https://www.gnu.org/licenses/>.
-}
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
    , toList
    , toLists
    , (.*)
    , (^*)
    , rref
    , submatrix
    ) where

import GHC.TypeLits (Nat, KnownNat, natVal, type (+), type (-), type (<=))
import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (mappend)
import Data.Maybe (listToMaybe)

import qualified Data.Matrix as M
import qualified System.Random as R

newtype Matrix (m :: Nat) (n :: Nat) (f :: *) = Matrix (M.Matrix f)
    deriving (Eq, Functor, Applicative, Foldable, Traversable, Monoid)

instance forall m n f. Show f => Show (Matrix m n f) where
    show (Matrix mat) = M.prettyMatrix mat

instance forall m n f. Ord f => Ord (Matrix m n f) where
    compare x y = toList x `compare` toList y -- TODO: Do not use `toList`?

instance forall f m n. Num f => Num (Matrix m n f) where
    (Matrix x) + (Matrix y) = Matrix $ x + y
    (Matrix x) - (Matrix y) = Matrix $ x - y
    (*) = error "Data.Matrix.Safe: (*) not allowed. Use (.*) instead"
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = Matrix . fromInteger

instance forall m n a. (KnownNat m, KnownNat n, R.Random a)
  => R.Random (Matrix m n a) where
      random g =
          let m = fromInteger . natVal $ Proxy @m
              n = fromInteger . natVal $ Proxy @n
              (g1,g2) = R.split g
              rmat = fromList . take (m*n) . R.randoms $ g1
           in (rmat, g2)
      randomR (lm,hm) g =
          -- lm and hm are matrices. We zip the elements and use these as
          -- hi/lo bounds for the random generator
          let m = fromInteger . natVal $ Proxy @m
              n = fromInteger . natVal $ Proxy @n
              zipEls :: [(a,a)]
              zipEls = zip (toList lm) (toList hm)
              rmatStep :: R.RandomGen g => (a,a) -> ([a],g) -> ([a],g)
              rmatStep hilo (as,g1) = let (a,g2) = R.randomR hilo g1
                                       in (a:as,g2)
              (rElList,g') = foldr rmatStep ([],g) zipEls
           in (fromList rElList,g')


-- | Type safe matrix multiplication
(.*) :: forall m k n a. Num a => Matrix m k a -> Matrix k n a -> Matrix m n a
(Matrix m) .* (Matrix n) = Matrix $ m * n

-- | Type safe scalar multiplication
(^*) :: forall m n a. Num a => a -> Matrix m n a -> Matrix m n a
x ^* (Matrix n) = Matrix $ M.scaleMatrix x n

type Vector = Matrix 1

matrix :: forall m n a. (KnownNat m, KnownNat n)
       => ((Int, Int) -> a) -> Matrix (m :: Nat) (n :: Nat) a
matrix = Matrix . M.matrix m' n'
    where m' = fromInteger $ natVal (Proxy @m)
          n' = fromInteger $ natVal (Proxy @n)

transpose :: forall m n a. Matrix m n a -> Matrix n m a
transpose (Matrix m) = Matrix . M.transpose $ m

(<|>) :: forall m n k a. (KnownNat n, KnownNat k)
      => Matrix m n a -> Matrix m k a -> Matrix m (k+n) a
(Matrix x) <|> (Matrix y) = Matrix $ x M.<|> y

identity :: forall n a. (Num a, KnownNat n) => Matrix n n a
identity = Matrix $ M.identity n'
    where n' = fromInteger $ natVal (Proxy @n)

zero :: forall m n a. (Num a, KnownNat n, KnownNat m) => Matrix m n a
zero = Matrix $ M.zero m' n'
    where n' = fromInteger $ natVal (Proxy @n)
          m' = fromInteger $ natVal (Proxy @m)

fromList :: forall m n a. (KnownNat m, KnownNat n) => [a] -> Matrix m n a
fromList as = if length as == n'*m'
                 then Matrix $ M.fromList m' n' as
                 else error $ "List has wrong dimension: "
                                <>show (length as)
                                <>" instead of "
                                <>show (n'*m')
    where n' = fromInteger $ natVal (Proxy @n)
          m' = fromInteger $ natVal (Proxy @m)

fromLists :: forall m n a. (KnownNat m, KnownNat n) => [[a]] -> Matrix m n a
fromLists as = if length as == m' && length (head as) == n'
                 then Matrix $ M.fromLists as
                 else error $ "List has wrong dimension: "
                                <>show (length as)<>":"
                                <>show (length $ head as)
                                <>" instead of "
                                <>show m' <>":"<> show n'
    where n' = fromInteger $ natVal (Proxy @n)
          m' = fromInteger $ natVal (Proxy @m)


toList :: forall m n a. Matrix m n a -> [a]
toList (Matrix m) = M.toList m


toLists :: forall m n a. Matrix m n a -> [[a]]
toLists (Matrix m) = M.toLists m


submatrix :: forall m n m' n' a.
    (KnownNat m, KnownNat n, KnownNat m', KnownNat n'
    , m' <= m, n' <= n)
      => Int -> Int -> Matrix m n a -> Matrix m' n' a
submatrix i j (Matrix mat) = Matrix $ M.submatrix i (i+m'-1) j (j+n'-1) mat
    where n' = fromInteger . natVal $ Proxy @n'
          m' = fromInteger . natVal $ Proxy @m'


rref :: forall m n a. (Fractional a, Eq a, KnownNat m, KnownNat n, m <= n)
     => Matrix m n a -> Either String (Matrix m n a)
rref (Matrix m) = Matrix <$> mrref m

-- | Take a matrix and convert it into row echelon form.
mrref :: (Fractional a, Eq a) => M.Matrix a -> Either String (M.Matrix a)
mrref m
    | M.ncols m < M.nrows m
        = Left $ "Invalid dimensions "
              ++ show (sizeStr (M.ncols m) (M.nrows m))
              ++ "; the number of columns must be greater than or equal to "
              ++ "the number of rows"
    | otherwise
        = rrefRefd =<< mref m
  where
    rrefRefd mtx
        | M.nrows mtx == 1    = Right mtx
        | otherwise =
            let -- this is super-slow: [resolvedRight] is cubic
                -- because [combineRows] is quadratic
                resolvedRight = foldr ((.) . resolveRow) id [1..col-1] mtx
                  where
                    col = M.nrows mtx
                    resolveRow n = M.combineRows n (-M.getElem n col mtx) col
                top = M.submatrix 1 (M.nrows resolvedRight - 1)
                          1 (M.ncols resolvedRight) resolvedRight
                top' = rrefRefd top
                bot = M.submatrix (M.nrows resolvedRight)
                          (M.nrows resolvedRight) 1
                          (M.ncols resolvedRight) resolvedRight
             in (M.<-> bot) <$> top'
    sizeStr n m = show n ++ "x" ++ show m

mref :: (Fractional a, Eq a) => M.Matrix a -> Either String (M.Matrix a)
mref mtx
    | M.nrows mtx == 1 = clearedLeft
    | otherwise = do
        (tl, tr, bl, br) <- M.splitBlocks 1 1 <$> clearedLeft
        br' <- mref br
        return $ (tl M.<|> tr) M.<-> (bl M.<|> br')
  where
    sigAtTop = (\row -> M.switchRows 1 row mtx) <$> goodRow
      where
        significantRow n = M.getElem n 1 mtx /= 0
        goodRow = case listToMaybe (filter significantRow [1..M.nrows mtx]) of
            Nothing -> Left "Attempt to invert a non-invertible matrix"
            Just x -> return x
    normalizedFirstRow = (\sigAtTop' ->
                M.scaleRow (1 / M.getElem 1 1 sigAtTop') 1 sigAtTop')
                    <$> sigAtTop
    clearedLeft = do
        comb <- mapM combinator [2..M.nrows mtx]
        firstRow <- normalizedFirstRow
        return $ foldr (.) id comb firstRow
      where
        combinator n = (\normalizedFirstRow' ->
            M.combineRows n (-M.getElem n 1 normalizedFirstRow') 1)
                <$> normalizedFirstRow

