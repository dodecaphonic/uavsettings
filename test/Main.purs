module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Exception (EXCEPTION)
import Test.QuickCheck (quickCheck)
import Prelude (Unit, bind, (<), (+), ($), (>), (&&))
import Coverage ( FocalLength
                , Sensor
                , UAVSettings
                , Meters
                , groundWidth
                , groundHeight
                , groundPixelSize
                , footprint)

phantomCamera :: Sensor
phantomCamera = { width: 6.17, height: 4.55 }

phantomLens :: FocalLength
phantomLens = 5.0

settings :: UAVSettings
settings =
  {
    sensor: phantomCamera
  , focalLength: phantomLens
  , imageDimensions: { width: 4384, height: 3288 }
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
    s  = settings { focalLength = fl }
    s' = settings { focalLength = fl + 10.0 }
    gwa = groundWidth $ footprint s
    gha = groundHeight $ footprint s
    gwb = groundWidth $ footprint s'
    ghb = groundHeight $ footprint s'

altitudeGrowsIncreasesPixelSize :: Meters -> Boolean
altitudeGrowsIncreasesPixelSize n = ps0 < ps1
  where
    ps0 = groundPixelSize $ settings { groundAltitude = n }
    ps1 = groundPixelSize $ settings { groundAltitude = (n + 10.0) }

main :: forall eff. Eff (console :: CONSOLE
                        , random :: RANDOM
                        , err :: EXCEPTION | eff) Unit
main = do
  quickCheck altitudeGrowsIncreasesPixelSize
  quickCheck focalLengthGrowthReducesGroundCoverage
