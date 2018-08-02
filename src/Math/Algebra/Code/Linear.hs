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
Maintainer  : wanja.hs@chrummibei.ch
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
>>> 2 ^* e4 :: Vector 7 F3
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
    , codeFromA

    , standardForm, standardFormGenerator

    -- * Code-Vectors and codewords
    , Vector, encode, isCodeword, hasError, weight, codewords

    -- * Decoding
    , syndrome, decode, syndromeDecode, calcSyndromeTable

    -- * Code transformers
    , dualCode, permuteCode

    -- * Special codes and their generators
    , trivialCode, simplex, hamming
    , BinaryCode

    -- * Helper functions
    , randomPermMatrix
    , codeLength
    , rank

    , e, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10
    , char

    -- * Reexported matrix functions from "Math.Algebra.Matrix"
    , matrix, zero, transpose, fromList, fromLists

    -- * Reexported finite fields from @Math.Algebra.Field@
    , F2, F3, F5, F7, F11
    , F4, F8, F16, F9
    ) where

-- Linear codes from mathematical coding theory, including error correcting
-- codes
import Prelude hiding (CVector)
import GHC.TypeLits
        ( Nat, KnownNat, natVal
        , type (<=), type (+), type (-), type (^)
        )

import Control.Monad.Random.Class (MonadRandom, getRandoms)
import Data.Bifunctor (first)
import Data.Either (fromRight)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.List (permutations)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import System.Random ( Random, RandomGen
                     , random, randomR, randoms, randomRs, split)

import Math.Core.Utils (FinSet, elts)
import Math.Combinat.Permutations (_randomPermutation)
import Math.Common.IntegerAsType (IntegerAsType, value)
import Math.Algebra.Field.Base
        ( FiniteField, eltsFq, basisFq, Fp(Fp)
        , F2, F3, F5, F7, F11
        )
import Math.Algebra.Field.Static (Size, Characteristic, PolyDegree, char)
import Math.Algebra.Field.Extension
        ( ExtensionField(Ext), x, embed, pvalue
        , F4, F8, F16, F9
        )
import Math.Algebra.Field.Instances -- import Random instances for Fields
import Math.Algebra.Matrix
    ( Matrix, matrix, transpose, (<|>), (.*)
    , identity, zero, fromList, fromLists, Vector, rref, submatrix
    )


-- | A 'Generator' is the generator matrix of a linear code, not necessarily
--   in standard form.
type Generator (n :: Nat) (k :: Nat) = Matrix k n

-- | A 'CheckMatrix' or parity matrix is the dual of a 'Generator'. It can
--   be used to check if a word is a valid code word for the code. Also,
--   \[ \forall v \in f^k: cG \cdot H^\top = 0 \]
--   i.e. the code is generated by the kernel of a check matrix.
type CheckMatrix (n :: Nat) (k :: Nat) = Matrix (n-k) n

-- | A \([n,k]\)-Linear code over the field @f@. The code parameters @f@,@n@ and
--   @k@ are carried on the type level.
--   A linear code is a subspace @C@ of \(f^n\) generated by the generator matrix.
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
instance forall n k f c.
    (KnownNat n, KnownNat k, KnownNat (Characteristic f))
    => Show (LinearCode n k f) where
        show LinearCode{distance=md} =
            '[':show n<>","<>show k<>dist<>"]_"<>show c<>"-Code"
                where c = char (Proxy :: Proxy f)
                      n = natToInt @n Proxy
                      k = natToInt @k Proxy
                      dist = fromMaybe "" $ fmap (\d -> ',':show d) md

instance forall n k f.
    (KnownNat n, KnownNat k, k <= n, Eq f, FinSet f, Num f, Ord f)
      => Bounded (LinearCode n k f) where
    minBound = trivialCode
    maxBound = codeFromA $ matrix (const $ last elts)


-- | A random permutation matrix
randomPermMatrix :: forall g n r. (KnownNat n, Num r, RandomGen g)
                 => g -> (Matrix n n r, g)
randomPermMatrix g =
    let n = natToInt @n Proxy
        delta i j = if i == j then 1 else 0
        (perm,g') = _randomPermutation n g
     in (fromLists [ [ delta i (perm !! (j-1))
                     | j <- [1..n] ]
                   | i <- [1..n] ],g')

-- | A random code with a generator in standard form. This does not generate
--   all possible codes but only one representant of the equivalence class
--   modulo similar codes.
randomStandardFormCode :: forall n k f g.
    ( KnownNat n, KnownNat k, k <= n
    , Eq f, FinSet f, Num f, Ord f, Random f, RandomGen g)
      => g -> (LinearCode n k f, g)
randomStandardFormCode = first codeFromA . randomAMatrix
  where
    randomAMatrix :: RandomGen g => g -> (Matrix k (n-k) f,g)
    randomAMatrix = random


instance forall n k f.
    ( KnownNat n, KnownNat k, k <= n
    , Eq f, FinSet f, Num f, Ord f, Random f)
  => Random (LinearCode n k f) where
      random g = uncurry shuffleCode $ randomStandardFormCode g

      randomR (hc,lc) g =
          let k = fromInteger . natVal $ Proxy @k
              n = fromInteger . natVal $ Proxy @n
              extractA = submatrix 1 k . generatorMatrix
              (rmat,g2) = randomR (extractA hc, extractA lc) g
              rcode = codeFromA rmat
           in shuffleCode rcode g2


-- | Uses Gaussian eleminiation via 'rref' from 'Data.Matrix.Safe' to
--   find the standard form of generators. This might fail since not all
--   codes can be converted to standard form without permutation of columns.
standardForm :: forall n k f.
    (Eq f, Fractional f, KnownNat n, KnownNat k, k <= n)
      => Generator n k f -> Generator n k f
standardForm = rref


-- | The standard from generator of a linear code. Uses 'standardForm' to
--   try to create a standard form generator which might fail.
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
weight :: forall n f m. (Eq f, Num f, Functor m, Foldable m) => m f -> Int
weight = sum . fmap (\x -> if x==0 then 0 else 1)

-- | Generate a linear [n,k]_q-Code over the field a with the generator in
--   standard form (I|A), where the given function generates the k×(n-k)-matrix
--   A.
codeFromA :: forall k n f.
    (KnownNat n, KnownNat k, k <= n, Eq f, FinSet f, Num f, Ord f)
      => Matrix k (n-k) f
            -- ^ Elements of A where top-left is (1,1) and bottom right (k,n-k)
      -> LinearCode n k f
codeFromA a = recalcSyndromeTable LinearCode
    { generatorMatrix = identity <|> a
    , checkMatrix = (-transpose a) <|> identity -- () are important for f/=F2
    , distance = Nothing
    , syndromeTable = undefined
    }


-- * Codewords and their properties

-- | Get the codeword generated by the given k-sized vector.
encode :: forall n k f. Num f => LinearCode n k f -> Vector k f -> Vector n f
encode code vs = vs .* generatorMatrix code


-- | List all vectors of length n over field f
allVectors :: forall n f. (KnownNat n, FinSet f, Num f, Eq f) => [Vector n f]
allVectors = fromList <$> allVectorsI (natToInt @n Proxy)

-- | List all lists given length over field f
allVectorsI :: forall f. (FinSet f, Num f, Eq f) => Int -> [[f]]
allVectorsI n = iterate addDim [[]] !! n
  where addDim vs = [ x:v | v <- vs, x <- elts ]

-- | List all vectors of length n with non-zero elements over field f
fullVectors :: forall n f. (KnownNat n, FinSet f, Num f, Eq f) => [Vector n f]
fullVectors = fromList <$> fullVectorsI (natToInt @n Proxy)

-- | List all vectors of given length with non-zero elements over field f
fullVectorsI :: forall f. (FinSet f, Num f, Eq f) => Int -> [[f]]
fullVectorsI n = iterate addDim [[]] !! n
  where addDim vs = [ x:v | v <- vs, x <- elts, x /= 0 ]

-- | List of all words with given hamming weight
hammingWords :: forall n f. (KnownNat n, FinSet f, Num f, Eq f)
    => Int -> [Vector n f]
hammingWords w = fromList <$> shuffledVecs
  where
    n = natToInt @n Proxy
    orderedVecs :: [[f]] -- [1,x,1,1,0..0]
    orderedVecs = (++) (replicate (n-w) 0) <$> fullVectorsI w
    shuffledVecs :: [[f]]
    shuffledVecs = orderedVecs >>= permutations

-- | List of all words with hamming weight smaller than a given boundary
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
          w = fromMaybe (n-k+1) $ distance c

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

-- |The dual code is the code generated by the check matrix
dualCode :: forall n k f.
    (KnownNat n, KnownNat k, k <= n, Eq f, FinSet f, Num f, Ord f)
      => LinearCode n k f -> LinearCode n (n-k) f
dualCode c = recalcSyndromeTable
                    LinearCode { generatorMatrix = checkMatrix c
                               , checkMatrix = generatorMatrix c
                               , distance = distance c
                               , syndromeTable = undefined
                               }


-- | Permute the rows of a code with a permutation matrix. The given permutation
--   matrix must be a valid permutation matrix; this is not checked.
--   This effectively multiplies the generator and check matrix from the right
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
--   positions of elements of all codewords
shuffleCode :: forall n k f g.
    (KnownNat n, KnownNat k, k <= n, RandomGen g, Eq f, FinSet f, Num f, Ord f)
      => LinearCode n k f -> g -> (LinearCode n k f, g)
shuffleCode c g =
    let (p,g') = randomPermMatrix g
     in (permuteCode c p, g')


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
    , 1 <= s^k, k <= s^k, 1+k <= s^k, Size (Fp p) ~ s)
        => LinearCode (s^k-1) k (Fp p)
simplex = codeFromA . transpose $ fromLists nonUnit
  where
    k = natToInt @k Proxy
    allVectors :: Size (Fp p) ~ s => [[Fp p]]
    allVectors = fmap reverse . tail $ iterate ([(0:),(1:)] <*>) [[]] !! k
    nonUnit = filter ((>1) . weight) allVectors

-- | The /Hamming(7,4)/-code. It is a [7,4,3]_2 code
hamming :: (KnownNat m, 2 <= m, m <= 2^m, 1+m <= 2^m)
        => LinearCode (2^m-1) (2^m-m-1) F2
hamming = dualCode simplex { distance = Just 3 }


-- * Helper functions

-- | Standard base vector [0..0,1,0..0] for any field. Parameter must be >=1
e :: forall n f. (KnownNat n, Num f) => Int -> Vector n f
e i = fromList $ replicate (i-1) 0 ++ 1 : replicate (n-i) 0
        where
          n = natToInt @n Proxy

-- | First base vector [1,0..0]
e1 :: forall n f. (KnownNat n, Num f) => Vector n f
e1 = e 1

-- | Second base vector [0,1,0..0]
e2 :: forall n f. (KnownNat n, Num f) => Vector n f
e2 = e 2

e3 :: forall n f. (KnownNat n, Num f) => Vector n f
e3 = e 3

e4 :: forall n f. (KnownNat n, Num f) => Vector n f
e4 = e 4

e5 :: forall n f. (KnownNat n, Num f) => Vector n f
e5 = e 5

e6 :: forall n f. (KnownNat n, Num f) => Vector n f
e6 = e 6

e7 :: forall n f. (KnownNat n, Num f) => Vector n f
e7 = e 7

e8 :: forall n f. (KnownNat n, Num f) => Vector n f
e8 = e 8

e9 :: forall n f. (KnownNat n, Num f) => Vector n f
e9 = e 9

e10 :: forall n f. (KnownNat n, Num f) => Vector n f
e10 = e 10

-- vim : set colorcolumn=80
