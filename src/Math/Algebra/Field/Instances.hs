{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
Module      : Math.Algebra.Field.Instances
Description : Missing instnaces for @HaskellForMaths@'s 'Math.Algebra.Field'
Copyright   : (c) Wanja Chresta, 2018
License     : GPL-3
Maintainer  : wanja dot hs at chrummibei dot ch
Stability   : experimental
Portability : POSIX

Some important instances like 'Random' and 'Bounded' are missing from
@HaskellForMath@'s implementation of finite fiels. Here, orphan instances
for these classes are added.
-}

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


