module UAVSettings.Coverage where

import Math (atan)
import Prelude
import Data.Int (toNumber)

import UAVSettings.Units
  ( Meters(Meters)
  , Centimeters(Centimeters)
  , Milimeters
  , Degrees
  , FocalLength
  , Pixels
  , MetersPerSecond
  , Seconds
  , metersToNumber
  , centimetersToNumber
  )

type Sensor = { width :: Milimeters, height :: Milimeters }

type ImageDimensions = { width :: Pixels, height :: Pixels }

type UAVSettings =
  { sensor :: Sensor
  , focalLength :: FocalLength
  , imageDimensions :: ImageDimensions
  , speed :: MetersPerSecond
  , captureInterval :: Seconds
  , shutterSpeed :: Int
  , gimbalX :: Number
  , gimbalY :: Number
  , groundAltitude :: Meters
  }

type Footprint = { width :: Meters, height :: Meters }

type Fov = { x :: Degrees, y :: Degrees }

captureInterval :: UAVSettings -> Seconds
captureInterval ({ captureInterval = c }) = case c of
  i | i <= 0.0 -> 10.0
  i -> i

shutterSpeed :: UAVSettings -> Int
shutterSpeed ({ shutterSpeed = s }) = case s of
  v | v <= 0 -> 1000
  v -> v

pixelWidth :: UAVSettings -> Pixels
pixelWidth s = s.imageDimensions.width

pixelHeight :: UAVSettings -> Pixels
pixelHeight s = s.imageDimensions.height

sensorDiagonal :: UAVSettings -> Milimeters
sensorDiagonal s =
  let
    x = s.sensor.width * s.sensor.width
    y = s.sensor.height * s.sensor.height
  in
    Math.sqrt $ x + y

diagonalDegrees :: UAVSettings -> Degrees
diagonalDegrees s = 180.0 * 2.0 * (x / Math.pi)
  where
    x = atan $ (sensorDiagonal s) / (2.0 * s.focalLength)

diagonalPixels :: UAVSettings -> Pixels
diagonalPixels s = Math.sqrt(px * px + py * py)
  where
    px = pixelWidth s
    py = pixelHeight s

diagonalMeters :: UAVSettings -> Meters
diagonalMeters s = alt * tan
  where
    alt = s.groundAltitude
    tan = Meters $ Math.tan $ Math.pi * (diagonalDegrees s) / (2.0 * 180.0)

imageIntervalMeters :: UAVSettings -> Meters
imageIntervalMeters s = Meters $ s.speed * (captureInterval s)

angularResolution :: UAVSettings -> Number
angularResolution s = (diagonalDegrees s) / (diagonalPixels s)

groundPixelSize :: UAVSettings -> Centimeters
groundPixelSize s = Centimeters gsd
  where
    dms = metersToNumber $ diagonalMeters s
    dpx = diagonalPixels s
    gsd = 100.0 * dms / dpx

motionBlurCentimeters :: UAVSettings -> Centimeters
motionBlurCentimeters s = Centimeters blur
  where
    blur = 100.0 * s.speed / (toNumber $ shutterSpeed s)

motionBlurPixels :: UAVSettings -> Number
motionBlurPixels s = centimetersToNumber $ motionBlurCentimeters s / groundPixelSize s

imageOverlapMeters :: UAVSettings -> Meters
imageOverlapMeters s = yMeters - interval
  where
    yMeters  = (footprint s).width
    interval = imageIntervalMeters s

imageOverlapPercent :: UAVSettings -> Number
imageOverlapPercent s = calc (imageOverlapMeters s) (footprint s).width
  where
    calc (Meters n) (Meters gh) = case n of
      v | v <= 0.0 -> 0.0
      v -> 100.0 * v / gh

fieldOfView :: UAVSettings -> Fov
fieldOfView ({ focalLength = fl, sensor = s }) = { x, y }
  where
    x = 2.0 * (degrees $ atan (s.width / (2.0 * fl)))
    y = 2.0 * (degrees $ atan (s.height / (2.0 * fl)))

footprint :: UAVSettings -> Footprint
footprint s = { width: Meters wide, height: Meters tall }
  where
    tall = pixelHeight s * (metersToNumber $ diagonalMeters s) / (diagonalPixels s)
    wide = pixelWidth s * (metersToNumber $ diagonalMeters s) / (diagonalPixels s)

degrees :: Number -> Number
degrees r = r * (180.0 / Math.pi)
