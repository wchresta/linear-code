{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
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
Module      : Math.Algebra.Field.Static
Description : Some type families extracting finite field parameters
Copyright   : (c) Wanja Chresta, 2018
License     : GPL-3
Maintainer  : wanja.hs@chrummibei.ch
Stability   : experimental
Portability : POSIX

Some finite field parameters are missing from @HaskellForMaths@ implementation.
Here, we add type classes to add these parameters to the type level.
-}
module Math.Algebra.Field.Static where

import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (Nat, KnownNat, type (^), natVal)
import qualified Math.Algebra.Field.Base as F
import qualified Math.Algebra.Field.Extension as F


-- | The characteristic of a finite field on the type level. The characteristic
--   is: For any element @x@ in the field @f@ with characteristic @c@, we have:
--   @c * x = x + x + .. + x (c times) = 0@
type family Characteristic (f :: *) :: Nat
type instance Characteristic F.F2 = 2
type instance Characteristic F.F3 = 3
type instance Characteristic F.F5 = 5
type instance Characteristic F.F7 = 7
type instance Characteristic F.F11 = 11
type instance Characteristic F.F13 = 13
type instance Characteristic F.F17 = 17
type instance Characteristic F.F19 = 19
type instance Characteristic F.F23 = 23
type instance Characteristic F.F29 = 29
type instance Characteristic F.F31 = 31
type instance Characteristic F.F37 = 37
type instance Characteristic F.F41 = 41
type instance Characteristic F.F43 = 43
type instance Characteristic F.F47 = 47
type instance Characteristic F.F53 = 53
type instance Characteristic F.F59 = 59
type instance Characteristic F.F61 = 61
type instance Characteristic F.F67 = 67
type instance Characteristic F.F71 = 71
type instance Characteristic F.F73 = 73
type instance Characteristic F.F79 = 79
type instance Characteristic F.F83 = 83
type instance Characteristic F.F89 = 89
type instance Characteristic F.F97 = 97
type instance Characteristic (F.ExtensionField k poly)
  = Characteristic k -- Extension fields have their base fields char


-- | Characteristic of a field. It takes a finite field type in the proxy
--   value and gives the characteristic. This is done using type families
--   To support new finite field types, you need to add a type instance
--   for the type family 'Characteristic'.
char :: forall c f. (KnownNat c, c ~ Characteristic f) => Proxy f -> Int
char Proxy = fromInteger . natVal $ Proxy @c


-- | Type family which gives the degree of a polynomial type. This is used to
--   extract type level information from 'Math.Algebra.Field.Extension'
type family PolyDegree (f :: *) :: Nat
type instance PolyDegree F.ConwayF4 = 2
type instance PolyDegree F.ConwayF8 = 3
type instance PolyDegree F.ConwayF9 = 2
type instance PolyDegree F.ConwayF16 = 4
type instance PolyDegree F.ConwayF25 = 2
type instance PolyDegree F.ConwayF27 = 3
type instance PolyDegree F.ConwayF32 = 5


-- | Type family which gives the size of a field, i.e. the number of elements
--   of a finite field.
type family Size (f :: *) :: Nat
type instance Size (F.Fp p) = Characteristic (F.Fp p)
type instance Size (F.ExtensionField fp poly) =
    Characteristic fp ^ PolyDegree poly


