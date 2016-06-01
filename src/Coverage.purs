module UAVSettings.Coverage where

import Math (atan)
import Prelude
import Data.Int (toNumber)

import UAVSettings.Units as U

type Sensor = { width :: U.Milimeters, height :: U.Milimeters }

type ImageDimensions = { width :: U.Pixels, height :: U.Pixels }

type UAVSettings =
  { sensor :: Sensor
  , focalLength :: U.FocalLength
  , imageDimensions :: ImageDimensions
  , speed :: U.MetersPerSecond
  , captureInterval :: U.Seconds
  , shutterSpeed :: Int
  , gimbalX :: Number
  , gimbalY :: Number
  , groundAltitude :: U.Meters
  }

type Footprint = { width :: U.Meters, height :: U.Meters }

type Fov = { x :: U.Degrees, y :: U.Degrees }

captureInterval :: UAVSettings -> U.Seconds
captureInterval ({ captureInterval = c }) = case c of
  i | i <= 0.0 -> 10.0
  i -> i

shutterSpeed :: UAVSettings -> Int
shutterSpeed ({ shutterSpeed = s }) = case s of
  v | v <= 0 -> 1000
  v -> v

pixelWidth :: UAVSettings -> U.Pixels
pixelWidth s = s.imageDimensions.width

pixelHeight :: UAVSettings -> U.Pixels
pixelHeight s = s.imageDimensions.height

sensorDiagonal :: UAVSettings -> U.Milimeters
sensorDiagonal s = let
  x = s.sensor.width * s.sensor.width
  y = s.sensor.height * s.sensor.height
  in Math.sqrt $ x + y

diagonalDegrees :: UAVSettings -> U.Degrees
diagonalDegrees s = 180.0 * 2.0 * (x / Math.pi)
  where
    x = atan $ (sensorDiagonal s) / (2.0 * s.focalLength)

diagonalPixels :: UAVSettings -> U.Pixels
diagonalPixels s = Math.sqrt(px * px + py * py)
  where
    px = pixelWidth s
    py = pixelHeight s

diagonalMeters :: UAVSettings -> U.Meters
diagonalMeters s = alt * tan
  where
    alt = s.groundAltitude
    tan = U.Meters $ Math.tan $ Math.pi * (diagonalDegrees s) / (2.0 * 180.0)

imageIntervalMeters :: UAVSettings -> U.Meters
imageIntervalMeters s = U.Meters $ s.speed * (captureInterval s)

angularResolution :: UAVSettings -> Number
angularResolution s = (diagonalDegrees s) / (diagonalPixels s)

groundPixelSize :: UAVSettings -> U.Centimeters
groundPixelSize s = 100.0 * ((U.metersToNumber $ diagonalMeters s) / diagonalPixels s)

motionBlurCentimeters :: UAVSettings -> U.Centimeters
motionBlurCentimeters s = 100.0 * s.speed / (toNumber $ shutterSpeed s)

motionBlurPixels :: UAVSettings -> Number
motionBlurPixels s = (100.0 * s.speed / (toNumber $ shutterSpeed s)) / (groundPixelSize s)

imageOverlapMeters :: UAVSettings -> U.Meters
imageOverlapMeters s = yMeters - interval
  where
    yMeters  = groundWidth $ footprint s
    interval = imageIntervalMeters s

imageOverlapPercent :: UAVSettings -> Number
imageOverlapPercent s = U.metersToNumber $ calc (imageOverlapMeters s) (groundWidth $ footprint s)
  where
    calc :: U.Meters -> U.Meters -> U.Meters
    calc (U.Meters n) gh = case n of
      v | v <= 0.0 -> U.Meters 0.0
      v -> (U.Meters 100.0) * (U.Meters v) / gh

fieldOfView :: UAVSettings -> Fov
fieldOfView ({ focalLength = fl, sensor = s }) = { x, y }
  where
    x = 2.0 * (degrees $ atan (s.width / (2.0 * fl)))
    y = 2.0 * (degrees $ atan (s.height / (2.0 * fl)))

footprint :: UAVSettings -> Footprint
footprint s = { width: wide, height: tall }
  where
    tall = U.Meters $ pixelHeight s * (U.metersToNumber $ diagonalMeters s) / (diagonalPixels s)
    wide = U.Meters $ pixelWidth s * (U.metersToNumber $ diagonalMeters s) / (diagonalPixels s)

groundHeight :: Footprint -> U.Meters
groundHeight f = f.height

groundWidth :: Footprint -> U.Meters
groundWidth f = f.width

degrees :: Number -> Number
degrees r = r * (180.0 / Math.pi)
