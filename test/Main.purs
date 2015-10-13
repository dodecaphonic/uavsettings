module Test.Main where

import Control.Monad.Eff.Console
import Test.QuickCheck
import Coverage
import Prelude

phantomCamera :: Sensor
phantomCamera = { width: 6.17, height: 4.55 }

phantomLens :: FocalLength
phantomLens = 5.0

settings :: UAVSettings
settings = {
  sensor: phantomCamera
  , focalLength: 30.0
  , imageDimensions: { width: 4384, height: 3288 }
  , speed: 6.0
  , captureInterval: 3.0
  , shutterSpeed: 250
  , gimbalX: 30.0
  , gimbalY: 30.0
  , groundAltitude: 100.0
}

uavAltitude a = settings { groundAltitude = a }

uavFocalLength a = UAVSettings {
  sensor: phantomCamera
  , focalLength: a
  , imageDimensions: ImageDimensions 4384 3288
  , speed: 6.0
  , captureInterval: 3.0
  , shutterSpeed: 250
  , gimbalX: 30.0
  , gimbalY: 30.0
  , groundAltitude: 100.0
}

focalLengthGrowthReducesGroundCoverage :: FocalLength -> Boolean
focalLengthGrowthReducesGroundCoverage fl =
  gwa > gwb && gha > ghb
  where
    s  = uavFocalLength fl
    s' = uavFocalLength $ fl + 10.0
    gwa = groundWidth $ footprint s
    gha = groundHeight $ footprint s
    gwb = groundWidth $ footprint s'
    ghb = groundHeight $ footprint s'

altitudeGrowsIncreasesPixelSize :: Meters -> Boolean
altitudeGrowsIncreasesPixelSize n = ps0 < ps1
  where
    ps0 = groundPixelSize $ uavAltitude n
    ps1 = groundPixelSize $ uavAltitude $ n + 10.0

main = do
  quickCheck altitudeGrowsIncreasesPixelSize
  quickCheck focalLengthGrowthReducesGroundCoverage
