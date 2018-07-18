{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Math.Algebra.Field.Static where

import GHC.TypeLits (Nat, type (^))
import qualified Math.Algebra.Field.Base as F
import qualified Math.Algebra.Field.Extension as F


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

type family PolyDegree (f :: *) :: Nat
type instance PolyDegree F.ConwayF4 = 2
type instance PolyDegree F.ConwayF8 = 3
type instance PolyDegree F.ConwayF9 = 2
type instance PolyDegree F.ConwayF16 = 4
type instance PolyDegree F.ConwayF25 = 2
type instance PolyDegree F.ConwayF27 = 3
type instance PolyDegree F.ConwayF32 = 5


type family Size (f :: *) :: Nat
type instance Size (F.Fp p) = Characteristic (F.Fp p)
type instance Size (F.ExtensionField fp poly) = Characteristic fp ^ PolyDegree poly


