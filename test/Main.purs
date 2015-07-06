module Test.Main where

import Control.Monad.Eff.Console
import Test.QuickCheck
import Coverage
import Prelude

phantomCamera :: Sensor
phantomCamera = Sensor 6.17 4.55

phantomLens :: FocalLength
phantomLens = 5.0

settings :: UAVSettings
settings = UAVSettings {
  sensor: phantomCamera
  , focalLength: 30.0
  , imageDimensions: ImageDimensions 4384 3288
  , speed: 6.0
  , captureInterval: 3.0
  , shutterSpeed: 250
  , gimbalX: 30.0
  , gimbalY: 30.0
  , groundAltitude: 100.0
}

uavAltitude a = UAVSettings {
  sensor: phantomCamera
  , focalLength: 30.0
  , imageDimensions: ImageDimensions 4384 3288
  , speed: 6.0
  , captureInterval: 3.0
  , shutterSpeed: 250
  , gimbalX: 30.0
  , gimbalY: 30.0
  , groundAltitude: a
}

altitudeGrowsIncreasesPixelSize :: Meters -> Boolean
altitudeGrowsIncreasesPixelSize n = ps0 < ps1
  where
    ps0 = groundPixelSize $ uavAltitude n
    ps1 = groundPixelSize $ uavAltitude $ n + 10.0

main = quickCheck altitudeGrowsIncreasesPixelSize
