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
Module      : Math.Algebra.Matrix
Description : Type safe matrix wrapper over the matrix library
Copyright   : (c) Wanja Chresta, 2018
License     : GPL-3
Maintainer  : wanja dot hs at chrummibei dot ch
Stability   : experimental
Portability : POSIX

Math.Algebra.Matrix wraps @matrix@'s Data.Matrix functions and adds size
information on the type level. Additionally, it fixes some issues that makes
the library work well with finite fields. The name of most functions is the
same as in Data.Matrix
-}

module Math.Algebra.Matrix
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

import GHC.TypeLits (Nat, KnownNat, natVal, type (+), type (<=))
import Data.List (find)
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>))
import Data.Maybe (isNothing)

import qualified Data.Matrix as M
import qualified System.Random as R


-- | A matrix over the type @f@ with @m@ rows and @n@ columns. This just wraps
--   the 'Data.Matrix.Matrix' constructor and adds size information to the type
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
          let zipEls :: [(a,a)]
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

-- | A row vector (a matrix with one row).
type Vector = Matrix 1

-- | /O(rows*cols)/. Generate a matrix from a generator function.
-- | The elements are 1-indexed, i.e. top-left element is @(1,1)@.
matrix :: forall m n a. (KnownNat m, KnownNat n)
       => ((Int, Int) -> a) -> Matrix (m :: Nat) (n :: Nat) a
matrix = Matrix . M.matrix m' n'
    where m' = fromInteger $ natVal @m Proxy
          n' = fromInteger $ natVal @n Proxy

-- | /O(rows*cols)/. The transpose of a matrix.
transpose :: forall m n a. Matrix m n a -> Matrix n m a
transpose (Matrix m) = Matrix . M.transpose $ m

-- | Horizontally join two matrices. Visually:
--
-- > ( A ) <|> ( B ) = ( A | B )
(<|>) :: forall m n k a. (KnownNat n, KnownNat k)
      => Matrix m n a -> Matrix m k a -> Matrix m (k+n) a
(Matrix x) <|> (Matrix y) = Matrix $ x M.<|> y

-- | /O(rows*cols)/. Identity matrix
identity :: forall n a. (Num a, KnownNat n) => Matrix n n a
identity = Matrix $ M.identity n'
    where n' = fromInteger $ natVal @n Proxy

-- | /O(rows*cols)/. The zero matrix
zero :: forall m n a. (Num a, KnownNat n, KnownNat m) => Matrix m n a
zero = Matrix $ M.zero m' n'
    where n' = fromInteger $ natVal @n Proxy
          m' = fromInteger $ natVal @m Proxy

-- | Create a matrix from a list of elements.
--   The list must have exactly length @n*m@. This is checked or else an 
--   exception is thrown.
fromList :: forall m n a. (KnownNat m, KnownNat n) => [a] -> Matrix m n a
fromList as = if length as == n*m
                 then Matrix $ M.fromList m n as
                 else error $ "List has wrong dimension: "
                                <>show (length as)
                                <>" instead of "
                                <>show (n*m)
  where n = fromInteger $ natVal @n Proxy
        m = fromInteger $ natVal @m Proxy

-- | Create a matrix from a list of rows. The list must have exactly @m@
--   lists of length @n@. An exception is thrown otherwise.
fromLists :: forall m n a. (KnownNat m, KnownNat n) => [[a]] -> Matrix m n a
fromLists as = if length as == m && all (\row -> length row == n) as
                 then Matrix $ M.fromLists as
                 else error $ "List has wrong dimension: "
                                <>show (length as)<>":"
                                <>show (length $ head as)
                                <>" instead of "
                                <>show m <>":"<> show n
    where n = fromInteger $ natVal @n Proxy
          m = fromInteger $ natVal @m Proxy

-- | Get the elements of a matrix stored in a list.
toList :: forall m n a. Matrix m n a -> [a]
toList (Matrix m) = M.toList m

-- | Get the elements of a matrix stored in a list of lists,
--   where each list contains the elements of a single row.
toLists :: forall m n a. Matrix m n a -> [[a]]
toLists (Matrix m) = M.toLists m


-- | /O(1)/. Extract a submatrix from the given position. The size of the
--   extract is determined by the types, i.e. the parameters define which
--   element is the top-left element of the extract.
--   CAUTION: It is not checked if an extract is possible. Wrong parameters
--   will cause an exception.
submatrix :: forall m n m' n' a.
    (KnownNat m, KnownNat n, KnownNat m', KnownNat n'
    , m' <= m, n' <= n)
      => Int -> Int -> Matrix m n a -> Matrix m' n' a
submatrix i j (Matrix mat) = Matrix $ M.submatrix i (i+m'-1) j (j+n'-1) mat
    where n' = fromInteger $ natVal @n' Proxy
          m' = fromInteger $ natVal @m' Proxy



-- | Reduced row echelon form. Taken from rosettacode. This is not the
--   implementation provided by the 'matrix' package.
--   https://rosettacode.org/wiki/Reduced_row_echelon_form#Haskell
rref :: forall m n a. (KnownNat m, KnownNat n, m <= n, Fractional a, Eq a)
     => Matrix m n a -> Matrix m n a
rref mat = fromLists $ f matM 0 [0 .. rows - 1]
  where 
    matM = toLists mat
    rows = length matM
    cols = length $ head matM

    f m _    []           = m
    f m lead (r : rs)
      | isNothing indices = m
      | otherwise         = f m' (lead' + 1) rs
      where 
        indices = find p l
        p (col, row) = m !! row !! col /= 0
        l = [(col, row) |
            col <- [lead .. cols - 1],
            row <- [r .. rows - 1]]

        Just (lead', i) = indices
        newRow = map (/ m !! i !! lead') $ m !! i

        m' = zipWith g [0..] $
            replace r newRow $
            replace i (m !! r) m
        g n row
            | n == r    = row
            | otherwise = zipWith h newRow row
              where h = subtract . (* row !! lead')

        replace :: Int -> b -> [b] -> [b]
        {- Replaces the element at the given index. -}
        replace n e t = a ++ e : b
          where (a, _ : b) = splitAt n t
