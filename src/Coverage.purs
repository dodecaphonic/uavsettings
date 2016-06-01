module Coverage where

import Math (atan)
import Prelude
import Data.Int (toNumber)

type Milimeters = Number
type Centimeters = Number
type Degrees = Number
type FocalLength = Milimeters
type Pixels = Int
type MetersPerSecond = Number
type Seconds = Number
type Meters = Number

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

sensor :: UAVSettings -> Sensor
sensor ({ sensor = s }) = s

focalLength :: UAVSettings -> FocalLength
focalLength ({ focalLength = l }) = l

focalSize :: UAVSettings -> FocalLength
focalSize s = s.focalLength

altitude :: UAVSettings -> Meters
altitude ({ groundAltitude = a }) = a

speed :: UAVSettings -> MetersPerSecond
speed ({ speed = s }) = s

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
sensorDiagonal s = let
  x = s.sensor.width * s.sensor.width
  y = s.sensor.height * s.sensor.height
  in Math.sqrt $ x + y

diagonalDegrees :: UAVSettings -> Degrees
diagonalDegrees s = 180.0 * 2.0 * (x / Math.pi)
  where
    x = atan $ (sensorDiagonal s) / (2.0 * s.focalLength)

diagonalPixels :: UAVSettings -> Meters
diagonalPixels s = Math.sqrt(toNumber $ px * px + py * py)
  where
    px = pixelWidth s
    py = pixelHeight s

diagonalMeters :: UAVSettings -> Meters
diagonalMeters s = alt * tan
  where
    alt = s.groundAltitude
    tan = Math.tan $ Math.pi * (diagonalDegrees s) / (2.0 * 180.0)

imageIntervalMeters :: UAVSettings -> Meters
imageIntervalMeters s = s.speed * (captureInterval s)

angularResolution :: UAVSettings -> Number
angularResolution s = (diagonalDegrees s) / (diagonalPixels s)

groundPixelSize :: UAVSettings -> Centimeters
groundPixelSize s = 100.0 * ((diagonalMeters s) / (diagonalPixels s))

motionBlurCentimeters :: UAVSettings -> Centimeters
motionBlurCentimeters s = 100.0 * s.speed / (toNumber $ shutterSpeed s)

motionBlurPixels :: UAVSettings -> Number
motionBlurPixels s = (100.0 * s.speed / (toNumber $ shutterSpeed s)) / (groundPixelSize s)

imageOverlapMeters :: UAVSettings -> Meters
imageOverlapMeters s = yMeters - interval
  where
    yMeters  = groundWidth $ footprint s
    interval = imageIntervalMeters s

imageOverlapPercent :: UAVSettings -> Meters
imageOverlapPercent s = calc (imageOverlapMeters s) (groundWidth $ footprint s)
  where
    calc m gh = case m of
      v | v <= 0.0 -> 0.0
      v -> 100.0 * v / gh

fieldOfView :: UAVSettings -> Fov
fieldOfView ({ focalLength = fl, sensor = s }) = { x, y }
  where
    x = 2.0 * (degrees $ atan (s.width / (2.0 * fl)))
    y = 2.0 * (degrees $ atan (s.height / (2.0 * fl)))

footprint :: UAVSettings -> Footprint
footprint s = { width: wide, height: tall }
  where
    tall = (toNumber $ pixelHeight s) * (diagonalMeters s) / (diagonalPixels s)
    wide = (toNumber $ pixelWidth s) * (diagonalMeters s) / (diagonalPixels s)

groundHeight :: Footprint -> Meters
groundHeight f = f.height

groundWidth :: Footprint -> Meters
groundWidth f = f.width

degrees :: Number -> Number
degrees r = r * (180.0 / Math.pi)
