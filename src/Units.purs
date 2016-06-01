module UAVSettings.Units where

import Prelude
  ( class Semiring
  , class ModuloSemiring
  , class Ring
  , class Show
  , class Ord
  , class Eq
  , mod
  , show
  , bind
  , return
  , compare
  , eq
  , (+)
  , (*)
  , ($)
  , (/)
  , (-)
  , (++)
  )
import Control.Monad.State (get, put)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (choose)


type Milimeters = Number
type Centimeters = Number
type Degrees = Number
type FocalLength = Milimeters
type Pixels = Number
type MetersPerSecond = Number
type Seconds = Number
newtype Meters = Meters Number

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
  show (Meters n) = (show n) ++ "m"

instance eqMeters :: Eq Meters where
  eq (Meters a) (Meters b) = eq a b

instance ordMeters :: Ord Meters where
  compare (Meters a) (Meters b) = compare a b

instance arbitraryMeters :: Arbitrary Meters where
  arbitrary = do
    n <- choose 0.0 2000.0
    return $ Meters n

metersToNumber :: Meters -> Number
metersToNumber (Meters n) = n
