{-# LANGUAGE ScopedTypeVariables #-}

module Math.Algebra.Field.Random where

import System.Random
import Data.Bifunctor (first)
import qualified Math.Algebra.Field.Base as F
import qualified Math.Algebra.Field.Extension as F
import qualified Math.Common.IntegerAsType as F
import qualified Math.Core.Utils as F

-- Type classes needed for the ExtensionField to become a FinSet
instance forall p. F.IntegerAsType p => Random (F.Fp p) where
        randomR = undefined -- FIXME

        random g = first (els !!) $ randomR (0,length els) g
            where els = F.elts

-- Type classes needed for the ExtensionField to become a FinSet
instance forall fp poly.
    (F.FinSet fp, Eq fp, Num fp, F.PolynomialAsType fp poly)
      => Random (F.ExtensionField fp poly) where
        randomR = undefined -- FIXME

        random g = first (els !!) $ randomR (0,length els) g
            where els = F.elts :: [F.ExtensionField fp poly]

