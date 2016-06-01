module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Exception (EXCEPTION)
import Test.QuickCheck (quickCheck)

import Prelude (class Ord, Unit, bind, (<), (+), ($), (>), (<=), (&&), (==))
import UAVSettings.Units (Meters(Meters), FocalLength)
import UAVSettings.Coverage
  ( Sensor
  , UAVSettings
  , groundWidth
  , groundHeight
  , groundPixelSize
  , footprint
  , imageOverlapMeters
  , imageOverlapPercent
  , shutterSpeed
  )

phantomCamera :: Sensor
phantomCamera = { width: 6.17, height: 4.55 }

phantomLens :: FocalLength
phantomLens = 5.0

settings :: UAVSettings
settings =
  {
    sensor: phantomCamera
  , focalLength: phantomLens
  , imageDimensions: { width: 4384.0, height: 3288.0 }
  , speed: 6.0
  , captureInterval: 3.0
  , shutterSpeed: 250
  , gimbalX: 30.0
  , gimbalY: 30.0
  , groundAltitude: Meters 100.0
}

shutterSpeedDefaultsWhenValueIsNegative :: Int -> Boolean
shutterSpeedDefaultsWhenValueIsNegative speed =
  if speed < 0 then usedSpeed == 1000 else usedSpeed == speed
  where
    settings' = settings { shutterSpeed = speed }
    usedSpeed = shutterSpeed settings'

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

altitudeIncrease :: forall a. (Ord a) => (UAVSettings -> a) -> Meters -> Meters -> Boolean
altitudeIncrease f alt inc =
  if alt + inc > alt then a < b else b <= a
  where
    a = f $ settings { groundAltitude = alt }
    b = f $ settings { groundAltitude = alt + inc }

altitudeChangesAffectPixelSize :: Meters -> Meters -> Boolean
altitudeChangesAffectPixelSize = altitudeIncrease groundPixelSize

altitudeChangesAffectImageOverlapMeters :: Meters -> Meters -> Boolean
altitudeChangesAffectImageOverlapMeters = altitudeIncrease imageOverlapMeters

altitudeChangesAffectImageOverlapPercent :: Meters -> Meters -> Boolean
altitudeChangesAffectImageOverlapPercent = altitudeIncrease imageOverlapPercent

main :: forall eff. Eff ( console :: CONSOLE
                        , random :: RANDOM
                        , err :: EXCEPTION | eff ) Unit
main = do
  quickCheck shutterSpeedDefaultsWhenValueIsNegative
  quickCheck focalLengthGrowthReducesGroundCoverage
  quickCheck altitudeChangesAffectPixelSize
  quickCheck altitudeChangesAffectImageOverlapMeters
  quickCheck altitudeChangesAffectImageOverlapPercent
