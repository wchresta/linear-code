{-# LANGUAGE ScopedTypeVariables #-}

module Math.Algebra.Field.Instances() where

import System.Random
import Data.Bifunctor (first)
import qualified Math.Algebra.Field.Base as F
import qualified Math.Algebra.Field.Extension as F
import qualified Math.Common.IntegerAsType as F
import qualified Math.Core.Utils as F

choose :: RandomGen g => [a] -> g -> (a,g)
choose [] = error "Cannot choose from empty list"
choose as = first (as !!) . randomR (0,length as-1)

-- Make prime fields Random
instance forall p. F.IntegerAsType p => Random (F.Fp p) where
    randomR (l,h) = choose $ filter (\x -> l <= x && x <= h 
                                        || l >= x && x >= h) F.elts
    random = choose F.elts

-- Make extension fields Random
instance forall fp poly.
    (F.FinSet fp, Ord fp, Num fp, F.PolynomialAsType fp poly)
      => Random (F.ExtensionField fp poly) where
        randomR (l,h) = choose $ filter (\x -> l <= x && x <= h 
                                            || l >= x && x >= h) F.elts
        random = choose F.elts

-- Make prime fields bounded
instance forall p. F.IntegerAsType p => Bounded (F.Fp p) where
    minBound = head F.elts
    maxBound = last F.elts


-- Make extension fields bounded
instance forall fp poly. 
    (F.FinSet fp, Eq fp, Num fp, F.PolynomialAsType fp poly) 
  => Bounded (F.ExtensionField fp poly) where
    minBound = head F.elts
    maxBound = last F.elts


