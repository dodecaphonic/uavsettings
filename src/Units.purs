module UAVSettings.Units
  ( Centimeters(Centimeters)
  , Meters(Meters)
  , Milimeters
  , Degrees
  , FocalLength
  , Pixels
  , MetersPerSecond
  , Seconds
  , metersToNumber
  , centimetersToNumber
  ) where

import Prelude
  ( class Semiring
  , class ModuloSemiring
  , class Ring
  , class Show
  , class Ord
  , class Eq
  , mod
  , show
  , compare
  , eq
  , (+)
  , (*)
  , ($)
  , (/)
  , (-)
  , (++)
  , (<$>)
  )
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (choose)

newtype Meters = Meters Number
newtype Centimeters = Centimeters Number

type Milimeters = Number
type Degrees = Number
type FocalLength = Milimeters
type Pixels = Number
type MetersPerSecond = Number
type Seconds = Number


instance metersSemiring :: Semiring Meters where
  add (Meters a) (Meters b) = Meters $ a + b
  mul (Meters a) (Meters b) = Meters $ a * b
  zero = Meters 0.0
  one = Meters 1.0

instance metersModuloSemiring :: ModuloSemiring Meters where
  div (Meters a) (Meters b) = Meters $ a / b
  mod (Meters a) (Meters b) = Meters $ a `mod` b

instance metersRing :: Ring Meters where
  sub (Meters a) (Meters b) = Meters $ a - b

instance metersShow :: Show Meters where
  show (Meters n) = (show n) ++ " m"

instance metersEq :: Eq Meters where
  eq (Meters a) (Meters b) = eq a b

instance metersOrd :: Ord Meters where
  compare (Meters a) (Meters b) = compare a b

instance centimetersShow :: Show Centimeters where
  show (Centimeters n) = (show n) ++ " cm"

instance centimetersEq :: Eq Centimeters where
  eq (Centimeters a) (Centimeters b) = eq a b

instance centimetersOrd :: Ord Centimeters where
  compare (Centimeters a) (Centimeters b) = compare a b

instance centimetersSemiring :: Semiring Centimeters where
  add (Centimeters a) (Centimeters b) = Centimeters $ a + b
  mul (Centimeters a) (Centimeters b) = Centimeters $ a * b
  zero = Centimeters 0.0
  one = Centimeters 1.0

instance centimetersModuloSemiring :: ModuloSemiring Centimeters where
  div (Centimeters a) (Centimeters b) = Centimeters $ a / b
  mod (Centimeters a) (Centimeters b) = Centimeters $ a `mod` b

instance metersArbitrary :: Arbitrary Meters where
  arbitrary = Meters <$> (choose 0.0 2000.0)

metersToNumber :: Meters -> Number
metersToNumber (Meters n) = n

centimetersToNumber :: Centimeters -> Number
centimetersToNumber (Centimeters n) = n
