{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
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
Module      : Math.Algebra.Code.Linear
Description : Linear codes over arbitrary fields
Copyright   : (c) Wanja Chresta, 2018
License     : GPL-3
Maintainer  : wanja dot hs at chrummibei dot ch
Stability   : experimental
Portability : POSIX

Naive implementation of coding theory linear codes and error correcting codes
over arbitrary fields, including finite fields. Goes well with the
@HaskellForMath@ library and its finite field implementations in
@Math.Algebra.Field@. To use extension fields (fields of prime power, i.e.
 \( F_{p^k} \) with \(k>1\), use one of the exported finite fields in
"Math.Algebra.Field.Extension" like 'F16' and its generator 'a16'.

As theoretical basis, Introduction to Coding Theory by Yehuda Lindell is used.
It can be found at
<http://u.cs.biu.ac.il/~lindell/89-662/coding_theory-lecture-notes.pdf>

= Usage

@
>>> :set -XDataKinds
>>> c <- randomIO :: IO (LinearCode 7 4 F5)
>>> c
[7,4]_5-Code
>>> generatorMatrix c
( 1 0 1 0 0 2 0 )
( 0 2 0 0 1 2 0 )
( 0 1 0 1 0 1 0 )
( 1 0 0 0 0 1 1 )
>>> e1 :: Vector 4 F5
( 1 0 0 0 )
>>> v = encode c e1
>>> v
( 1 0 1 0 0 2 0 )
>>> 2 ^* e4 :: Vector 7 F5
( 0 0 0 2 0 0 0 )
>>> vWithError = v + 2 ^* e4
>>> vWithError
( 1 0 1 2 0 2 0 )
>>> isCodeword c v
True
>>> isCodeword c vWithError
False
>>> decode c vWithError
Just ( 1 0 2 2 2 2 0 )
@

Notice, the returned vector is NOT the one without error. The reason for this
is that a random code most likely does not have a distance >2 which would be
needed to correct one error. Let's try with a hamming code

@
>>> c = hamming :: BinaryCode 7 4
>>> generatorMatrix c
( 1 1 0 1 0 0 0 )
( 1 0 1 0 1 0 0 )
( 0 1 1 0 0 1 0 )
( 1 1 1 0 0 0 1 )
>>> v = encode c e2
>>> vWithError = v + e3
>>> Just v' = decode c vWithError
>>> v' == v
True
@

-}
module Math.Algebra.Code.Linear
    ( LinearCode (..)
    , Generator, CheckMatrix
    , codeFromA, codeFromAD

    , standardForm, standardFormGenerator

    -- * Code-Vectors and codewords
    , Vector, encode, isCodeword, hasError, weight, codewords
    , allVectors, fullVectors, hammingWords, lighterWords

    -- * Decoding
    , syndrome, decode, syndromeDecode, calcSyndromeTable, recalcSyndromeTable
    , SyndromeTable

    -- * Code transformers
    , dualCode, dualCodeD, permuteCode, extendCode

    -- * Special codes and their generators
    , trivialCode, simplex, hamming, golay
    , BinaryCode

    -- * Helper functions
    , randomPermMatrix
    , codeLength
    , rank

    , eVec, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10
    , char

    -- * Reexported finite fields from @Math.Algebra.Field@
    , F2, F3, F5, F7, F11
    , F4, F8, F16, F9
    ) where

-- Linear codes from mathematical coding theory, including error correcting
-- codes
import GHC.TypeLits
        ( Nat, KnownNat, natVal
        , type (<=), type (+), type (-), type (^)
        )

import Data.Bifunctor (first)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.List (find, permutations)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import System.Random (Random, RandomGen, random, randoms, randomR, split)
import System.Random.Shuffle (shuffle')

import Math.Core.Utils (FinSet, elts)
import Math.Common.IntegerAsType (IntegerAsType)
import Math.Algebra.Field.Base (Fp, F2, F3, F5, F7, F11)
import Math.Algebra.Field.Static (Size, Characteristic, char)
import Math.Algebra.Field.Extension (F4, F8, F16, F9)
import Math.Algebra.Field.Instances () -- import Random instances for Fields
import Data.Matrix.Static
    ( Matrix, matrix, transpose, (<|>), (<->), (.*)
    , identity, zero, fromListUnsafe, fromListsUnsafe, toList, toLists
    , submatrix
    )


-- | A 'Generator' is the generator matrix of a linear code, not necessarily
--   in standard form.
type Generator (n :: Nat) (k :: Nat) = Matrix k n

-- | A 'CheckMatrix' or parity matrix is the dual of a 'Generator'. It can
--   be used to check if a word is a valid code word for the code. Also,
--   \[ \forall v \in f^k: cG \cdot H^\top = 0 \]
--   i.e. the code is generated by the kernel of a check matrix.
type CheckMatrix (n :: Nat) (k :: Nat) = Matrix (n-k) n

-- | For convenience, Vector is a one-row Matrix
type Vector = Matrix 1

-- | A \([n,k]\)-Linear code over the field @f@. The code parameters @f@,@n@ and
--   @k@ are carried on the type level.
--   A linear code is a subspace @C@ of \(f^n\) generated by the generator
--   matrix.
data LinearCode (n :: Nat) (k :: Nat) (f :: *)
    = LinearCode { generatorMatrix :: Generator n k f
                 -- ^ Generator matrix, used for most of the operations
                 , checkMatrix :: CheckMatrix n k f
                 -- ^ Check matrix which can be automatically calculated
                 --   from the standard form generator.
                 , distance :: Maybe Int
                 -- ^ The minimal distance of the code. This is the parameter
                 --   \(d\) in \([n,k,d]_q\) notation of code parameters. The
                 --   problem of finding the minimal distance is NP-Hard, thus
                 --   might not be available.
                 , syndromeTable :: SyndromeTable n k f
                 -- ^ A map of all possible syndromes to their error vector.
                 --   It is used to use syndrome decoding, a very slow decoding
                 --   algorithm.
                 }

-- | Extract an Int from a type level 'KnownNat'.
natToInt :: forall k. KnownNat k => Proxy k -> Int
natToInt = fromInteger . natVal

instance forall n k f. (Eq f, Fractional f, KnownNat n, KnownNat k, k <= n)
  => Eq (LinearCode n k f) where
    c == d = standardFormGenerator c == standardFormGenerator d

-- We do not show d since it might be expensive to calculate
instance forall n k f.
    (KnownNat n, KnownNat k, KnownNat (Characteristic f))
    => Show (LinearCode n k f) where
        show LinearCode{distance=md} =
            '[':show n<>","<>show k<>dist<>"]_"<>show c<>"-Code"
                where c = char (Proxy :: Proxy f)
                      n = natToInt @n Proxy
                      k = natToInt @k Proxy
                      dist = maybe "" (\d -> ',':show d) md

instance forall n k f.
    (KnownNat n, KnownNat k, k <= n, Eq f, FinSet f, Num f, Ord f)
      => Bounded (LinearCode n k f) where
    minBound = trivialCode
    maxBound = codeFromAD (Just 1) $ matrix (const $ last elts)


-- | A random permutation matrix
randomPermMatrix :: forall g n r. (KnownNat n, Num r, RandomGen g)
                 => g -> (Matrix n n r, g)
randomPermMatrix g =
    let n = natToInt @n Proxy
        delta i j = if i == j then 1 else 0
        (g1,g2) = split g
        perm = shuffle' [1..n] n g1
     in (fromListsUnsafe [ [ delta i (perm !! (j-1))
                           | j <- [1..n] ]
                         | i <- [1..n] ],g2)

-- | A random code with a generator in standard form. This does not generate
--   all possible codes but only one representant of the equivalence class
--   modulo similar codes.
randomStandardFormCode :: forall n k f g.
    ( KnownNat n, KnownNat k, k <= n
    , Eq f, FinSet f, Num f, Ord f, Random f, RandomGen g)
      => g -> (LinearCode n k f, g)
randomStandardFormCode = first (codeFromA . getRMat) . randomAMatrix
  where
    randomAMatrix :: RandomGen g => g -> (RMat k (n-k) f,g)
    randomAMatrix = random

-- Newtype for Random instances for Matrix to avoid orphans
newtype RMat m n a = RMat { getRMat :: Matrix m n a }
  deriving (Eq, Ord)

instance forall m n a. (KnownNat m, KnownNat n, Random a)
    => Random (RMat m n a) where
        random g =
            let m = fromInteger . natVal $ Proxy @m
                n = fromInteger . natVal $ Proxy @n
                (g1,g2) = split g
                rmat = fromListUnsafe . take (m*n) . randoms $ g1
             in (RMat rmat, g2)
        randomR (RMat lm, RMat hm) g =
            -- lm and hm are matrices. We zip the elements and use these as
            -- hi/lo bounds for the random generator
            let zipEls :: [(a,a)]
                zipEls = zip (toList lm) (toList hm)
                rmatStep :: RandomGen g => (a,a) -> ([a],g) -> ([a],g)
                rmatStep hilo (as,g1) = let (a,g2) = randomR hilo g1
                                         in (a:as,g2)
                (rElList,g') = foldr rmatStep ([],g) zipEls
             in (RMat $ fromListUnsafe rElList,g')

instance forall n k f.
    ( KnownNat n, KnownNat k, 1 <= k, k+1 <= n
    -- These are trivial deductions from the above; GHC<8.4 needs them
    , k <= n
    , Eq f, FinSet f, Num f, Ord f, Random f)
    => Random (LinearCode n k f) where
        random g = uncurry shuffleCode $ randomStandardFormCode g

        randomR (hc,lc) g =
            let extractA = RMat . submatrix @1 @(k+1) @k @n . generatorMatrix
                (RMat rmat,g2) = randomR (extractA hc, extractA lc) g
                rcode = codeFromA rmat
             in shuffleCode rcode g2


-- | Uses Gaussian eleminiation via 'rref' from 'Math.Algebra.Matrix' to
--   find the standard form of generators.
standardForm :: forall n k f.
    (Eq f, Fractional f, KnownNat n, KnownNat k, k <= n)
      => Generator n k f -> Generator n k f
standardForm = rrefFixed


-- | The standard from generator of a linear code. Uses 'standardForm' to
--   calculate a standard form generator.
standardFormGenerator :: forall n k f.
    (Eq f, Fractional f, KnownNat n, KnownNat k, k <= n)
      => LinearCode n k f -> Generator n k f
standardFormGenerator = standardForm . generatorMatrix


-- | Convenience function to extract the length @n@ from the type level
codeLength :: forall n k f. KnownNat n => LinearCode n k f -> Int
codeLength _ = natToInt @n Proxy

-- | Convenience function to extract the rank @k@ from the type level.
rank :: forall n k f. KnownNat k => LinearCode n k f -> Int
rank _ = natToInt @k Proxy

-- | The hamming weight of a Vector is an 'Int' between 0 and n
weight :: forall f m. (Eq f, Num f, Functor m, Foldable m) => m f -> Int
weight = sum . fmap (\x -> if x==0 then 0 else 1)

-- | Generate a linear \( [n,k]_q \)-Code over the field @f@ with the
--   generator in standard form @(I|A)@, where the given function generates
--   the \( k \times (n-k) \)-matrix A.
--   The distance is unknown for this code and thus decoding algorithms may
--   be very inefficient.
codeFromA :: forall k n f.
    (KnownNat n, KnownNat k, k <= n, Eq f, FinSet f, Num f, Ord f)
      => Matrix k (n-k) f
            -- ^ Elements of A where top-left is (1,1) and bottom right (k,n-k)
      -> LinearCode n k f
codeFromA = codeFromAD Nothing


-- | Generate a linear \( [n,k,d]_q \)-Code over the field @f@ with the
--   generator in standard form @(I|A)@, where the given function generates
--   the \( k \times (n-k) \)-matrix A.
codeFromAD :: forall k n f.
    (KnownNat n, KnownNat k, k <= n, Eq f, FinSet f, Num f, Ord f)
      => Maybe Int -- ^ Distance of the code. Give Nothing if it is unknown
      -> Matrix k (n-k) f
            -- ^ Elements of A where top-left is (1,1) and bottom right (k,n-k)
      -> LinearCode n k f
codeFromAD d a = recalcSyndromeTable LinearCode
    { generatorMatrix = identity <|> a
    , checkMatrix = (-transpose a) <|> identity -- () are important for f/=F2
    , distance = d
    , syndromeTable = undefined
    }


-- * Codewords and their properties

-- | Get the codeword generated by the given k-sized vector.
encode :: forall n k f. Num f => LinearCode n k f -> Vector k f -> Vector n f
encode code vs = vs .* generatorMatrix code


-- | List all vectors of length n over field f
allVectors :: forall n f. (KnownNat n, FinSet f, Num f, Eq f) => [Vector n f]
allVectors = fromListUnsafe <$> allVectorsI (natToInt @n Proxy)

-- | List all lists given length over field f
allVectorsI :: forall f. (FinSet f, Num f, Eq f) => Int -> [[f]]
allVectorsI n = iterate addDim [[]] !! n
  where addDim vs = [ x:v | v <- vs, x <- elts ]

-- | List all vectors of length n with non-zero elements over field f
fullVectors :: forall n f. (KnownNat n, FinSet f, Num f, Eq f) => [Vector n f]
fullVectors = fromListUnsafe <$> fullVectorsI (natToInt @n Proxy)

-- | List all vectors of given length with non-zero elements over field f
fullVectorsI :: forall f. (FinSet f, Num f, Eq f) => Int -> [[f]]
fullVectorsI n = iterate addDim [[]] !! n
  where addDim vs = [ x:v | v <- vs, x <- elts, x /= 0 ]

-- | List of all words with given hamming weight
hammingWords :: forall n f. (KnownNat n, FinSet f, Num f, Eq f)
    => Int -> [Vector n f]
hammingWords w = fromListUnsafe <$> shuffledVecs
  where
    n = natToInt @n Proxy
    orderedVecs :: [[f]] -- [1,x,1,1,0..0]
    orderedVecs = (++) (replicate (n-w) 0) <$> fullVectorsI w
    shuffledVecs :: [[f]]
    shuffledVecs = orderedVecs >>= permutations

-- | List of all words with (non-zero) hamming weight smaller than a given
--   boundary
lighterWords :: forall n f. (KnownNat n, FinSet f, Num f, Eq f)
    => Int -> [Vector n f]
lighterWords w = concat [ hammingWords l | l <- [1..w] ]

-- | A list of all codewords
codewords :: forall n k f.
  (KnownNat n, KnownNat k, k <= n, Num f, Eq f, FinSet f)
    => LinearCode n k f -> [Vector n f]
codewords c = map (encode c) allVectors

-- | Give the syndrome of a word for the given code. This is 0 if the word
--   is a valid code word.
syndrome :: forall n k f. Num f
         => LinearCode n k f -> Vector n f -> Syndrome n k f
syndrome c w = w .* transpose (checkMatrix c)

-- | Uses the exponential-time syndrome decoding algorithm for general codes.
--   c.f: https://en.wikipedia.org/wiki/Decoding_methods#Syndrome_decoding
syndromeDecode :: forall n k f.
    (KnownNat n, KnownNat k, k <= n, Ord f, Num f)
      => LinearCode n k f -> Vector n f -> Maybe (Vector n f)
syndromeDecode c w =
    let syn = syndrome c w
        e = M.lookup syn $ syndromeTable c
     in (w+) <$> e

-- | Synonym for syndromeDecoding, an inefficient decoding algorithm that works
--   for all linear codes.
decode :: forall n k f.
    (KnownNat n, KnownNat k, k <= n, Ord f, Num f)
      => LinearCode n k f -> Vector n f -> Maybe (Vector n f)
decode = syndromeDecode

-- | Pairs of (e,S(e)) where e is an error vector and S(e) is its syndrome.
type Syndrome n k f = Vector (n-k) f

-- | A syndrome table is a map from syndromes to their minimal weight
--   representative. Every vector @v@ has a syndrome \( S(v) \). This table
--   reverses the syndrome function @S@ and chooses the vector with the smallest
--   hamming weight from it's image. This is a lookup table for syndrome
--   decoding.
type SyndromeTable n k f = M.Map (Syndrome n k f) (Vector n f)

-- | Return a syndrome table for the given linear code. If the distance is not
--   known (i.e. 'minDist' @c@ = Nothing) this is very inefficient.
calcSyndromeTable :: forall n k f.
    (KnownNat n, KnownNat k, k <= n, Eq f, FinSet f, Num f, Ord f)
      => LinearCode n k f -> SyndromeTable n k f
-- We need to build a syndrome table for all codewords of wgt < floor $ (d-1)/2
-- If we do not know the weight (because distance code = Nothing), we assume
-- the worst case with a maximum distance of n-k+1
calcSyndromeTable c = M.fromListWith minWt allSyndromes
    where minWt x y = if weight x < weight y then x else y
          n = natToInt $ Proxy @n
          k = natToInt $ Proxy @k
          w = maybe (n-k+1) (\d -> div (d-1) 2) $ distance c

          allSyndromes :: [(Syndrome n k f, Vector n f)]
          allSyndromes = [(syndrome c e,e) | e <- lighterWords w]

-- | Replace the 'syndromeTable' of a code with a newly calculated syndrome
--   table for the (current) generator. Useful to get a syndrome table for
--   transformed codes when the table cannot be transformed, too.
recalcSyndromeTable :: forall n k f.
    (KnownNat n, KnownNat k, k <= n, Eq f, FinSet f, Num f, Ord f)
      => LinearCode n k f -> LinearCode n k f
recalcSyndromeTable c = c { syndromeTable = calcSyndromeTable c }


-- | Check if the given candidate code word is a valid code word for the
--   given linear code. If not, the party check failed.
isCodeword :: forall n k f. (Eq f, Num f, KnownNat n, KnownNat k, k <= n)
           => LinearCode n k f -> Vector n f -> Bool
isCodeword c w = syndrome c w == zero


-- | Check if the given candidate code word has errors, i.e. if some element
--   changed during transmission. This is equivalent with @not@ 'isCodeword'
hasError :: forall n k f. (Eq f, Num f, KnownNat n, KnownNat k, k <= n)
         => LinearCode n k f -> Vector n f -> Bool
hasError g = not . isCodeword g


-- * Code transformers

-- | The dual code is the code generated by the check matrix
--
--   This drops already calculated syndromeTables.
dualCode :: forall n k f.
    (KnownNat n, KnownNat k, k <= n, Eq f, FinSet f, Num f, Ord f)
      => LinearCode n k f -> LinearCode n (n-k) f
dualCode = dualCodeD Nothing


-- | The dual code is the code generated by the check matrix.
--
--   This drops already calculated syndromeTables.
dualCodeD :: forall n k f.
    (KnownNat n, KnownNat k, k <= n, Eq f, FinSet f, Num f, Ord f)
      => Maybe Int -- ^ The distance of the new code (if known) or Nothing
      -> LinearCode n k f -> LinearCode n (n-k) f
dualCodeD d c = recalcSyndromeTable
                    LinearCode { generatorMatrix = checkMatrix c
                               , checkMatrix = generatorMatrix c
                               , distance = d
                               , syndromeTable = undefined
                               }


-- | Permute the rows of a code with a permutation matrix. The given permutation
--   matrix must be a valid permutation matrix; this is not checked.
--   This effectively multiplies the generator and check matrix from the right.
--   Te distance of the resulting code stays the same.
--
--   This drops already calculated syndromeTables.
permuteCode :: forall n k f.
    (KnownNat n, KnownNat k, k <= n, Eq f, FinSet f, Num f, Ord f)
      => LinearCode n k f -> Matrix n n f -> LinearCode n k f
permuteCode c p = recalcSyndromeTable
                      LinearCode { generatorMatrix = generatorMatrix c .* p
                                 , checkMatrix = checkMatrix c .* p
                                 , distance = distance c
                                 , syndromeTable = undefined
                                 -- TODO: Permute syndrome table
                                 }


-- | Randomly permute the elements of the code. This is a shuffle of the
--   positions of elements of all codewords. The distance of the resulting
--   code stays the same.
--
--   This drops already calculated syndromeTables.
shuffleCode :: forall n k f g.
    (KnownNat n, KnownNat k, k <= n, RandomGen g, Eq f, FinSet f, Num f, Ord f)
      => LinearCode n k f -> g -> (LinearCode n k f, g)
shuffleCode c g =
    let (p,g') = randomPermMatrix g
     in (permuteCode c p, g')


-- | Extend the given code \( c \) by zero-columns. Vectors
--   \( v_{ext} \in c_{ext} \) have the form
--   \( v = (v_1, \dots , v_n, 0, \dots, 0) \) . The distance of the extended
--   code stays the same.
--   This drops a calculated syndromeTable and makes it necessary to recalculate
--   it if it's accessed.
extendCode :: forall n k f r.
    (KnownNat n, KnownNat k, KnownNat r, k <= n, 1 <= r, k <= n+r
    , Num f, Ord f, FinSet f)
      => LinearCode n k f -> LinearCode (n+r) k f
extendCode c = recalcSyndromeTable LinearCode
    { generatorMatrix = generatorMatrix c <|> zero :: Generator (n+r) k f
    , checkMatrix = (checkMatrix c <|> (zero :: Matrix (n-k) r f))
                    <->
                    ((zero :: Matrix r n f) <|> (identity :: Matrix r r f))
    , distance = distance c
    , syndromeTable = undefined
    }


-- * Special codes and their generators

-- | A binary code is a linear code over the field GF(2)
type BinaryCode n k = LinearCode n k F2

-- | The trivial code is the identity code where the parity bits are all zero.
trivialCode :: forall n k f.
    (KnownNat n, KnownNat k, k <= n, Eq f, FinSet f, Num f, Ord f)
      => LinearCode n k f
trivialCode = codeFromA (zero :: Matrix k (n-k) f)


-- | A simplex code is a code generated by all possible codewords consisting
--   of 0's and 1's except the zero vector.
simplex :: forall k p s.
    ( KnownNat s, KnownNat k , IntegerAsType p
    , 1 <= s^k, k <= s^k, 1 <= s^k-k, k <= s^k-1, Size (Fp p) ~ s)
        => LinearCode (s^k-1) k (Fp p)
simplex = codeFromA . transpose $ fromListsUnsafe nonUnit
  where
    k = natToInt @k Proxy
    nonUnit = filter ((>1) . weight) $ allVectorsI k

-- | The /Hamming(7,4)/-code. It is a [7,4,3]_2 code
hamming :: (KnownNat m, 2 <= m, m <= 2^m, m <= 2^m-1, 1 <= 2^m-m)
        => LinearCode (2^m-1) (2^m-m-1) F2
hamming = dualCodeD (Just 3) simplex


-- | The _Golay_-code is a perfect [24,12,7]-code.
--   It is the only other non-trivial perfect code and the only perfect code
--   that is able to correct 3 errors.
--
--   Syndrome decoding on this code takes a very, very long time.
golay :: LinearCode 23 12 F2
golay = codeFromAD (Just 7) golayA
  where
    golayA = fromListUnsafe
        [0,1,1,1,1,1,1,1,1,1,1
        ,1,1,1,0,1,1,1,0,0,0,1
        ,1,1,0,1,1,1,0,0,0,1,0
        ,1,0,1,1,1,0,0,0,1,0,1
        ,1,1,1,1,0,0,0,1,0,1,1
        ,1,1,1,0,0,0,1,0,1,1,0
        ,1,1,0,0,0,1,0,1,1,0,1
        ,1,0,0,0,1,0,1,1,0,1,1
        ,1,0,0,1,0,1,1,0,1,1,1
        ,1,0,1,0,1,1,0,1,1,1,0
        ,1,1,0,1,1,0,1,1,1,0,0
        ,1,0,1,1,0,1,1,1,0,0,0
        ]

-- * Helper functions

-- | Standard base vector [0..0,1,0..0] for any field. Parameter must be >=1
eVec :: forall n f. (KnownNat n, Num f) => Int -> Vector n f
eVec i = fromListUnsafe $ replicate (i-1) 0 ++ 1 : replicate (n-i) 0
           where
             n = natToInt @n Proxy

-- | First base vector [1,0..0]
e1 :: forall n f. (KnownNat n, Num f) => Vector n f
e1 = eVec 1

-- | Second base vector [0,1,0..0]
e2 :: forall n f. (KnownNat n, Num f) => Vector n f
e2 = eVec 2

e3 :: forall n f. (KnownNat n, Num f) => Vector n f
e3 = eVec 3

e4 :: forall n f. (KnownNat n, Num f) => Vector n f
e4 = eVec 4

e5 :: forall n f. (KnownNat n, Num f) => Vector n f
e5 = eVec 5

e6 :: forall n f. (KnownNat n, Num f) => Vector n f
e6 = eVec 6

e7 :: forall n f. (KnownNat n, Num f) => Vector n f
e7 = eVec 7

e8 :: forall n f. (KnownNat n, Num f) => Vector n f
e8 = eVec 8

e9 :: forall n f. (KnownNat n, Num f) => Vector n f
e9 = eVec 9

e10 :: forall n f. (KnownNat n, Num f) => Vector n f
e10 = eVec 10


------------------------
-- There is a bug in Data.Matrix's rref. So we need to implement our own
-- version until it's fixed.

-- | Reduced row echelon form. Taken from rosettacode. This is not the
--   implementation provided by the 'matrix' package.
--   https://rosettacode.org/wiki/Reduced_row_echelon_form#Haskell
rrefFixed :: forall m n a. (KnownNat m, KnownNat n, m <= n, Fractional a, Eq a)
          => Matrix m n a -> Matrix m n a
rrefFixed mat = fromListsUnsafe $ f matM 0 [0 .. rows - 1]
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



-- vim : set colorcolumn=80
