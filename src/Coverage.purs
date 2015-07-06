module Coverage where

import Math
import Prelude
import Data.Int (toNumber)

type Milimeters = Number
type Centimeters = Number
type Meters = Number
type Degrees = Number
type FocalLength = Milimeters
type Pixels = Int
type MetersPerSecond = Number
type Seconds = Number

data Sensor = Sensor Milimeters Milimeters

data ImageDimensions = ImageDimensions Pixels Pixels

data UAVSettings = UAVSettings {
  sensor :: Sensor
  , focalLength :: FocalLength
  , imageDimensions :: ImageDimensions
  , speed :: MetersPerSecond
  , captureInterval :: Seconds
  , shutterSpeed :: Int
  , gimbalX :: Number
  , gimbalY :: Number
  , groundAltitude :: Meters
}

data Footprint = Footprint Meters Meters

data Fov = Fov Degrees Degrees

sensor :: UAVSettings -> Sensor
sensor (UAVSettings { sensor = s }) = s

focalLength :: UAVSettings -> FocalLength
focalLength (UAVSettings { focalLength = l }) = l

altitude :: UAVSettings -> Meters
altitude (UAVSettings { groundAltitude = a }) = a

speed :: UAVSettings -> MetersPerSecond
speed (UAVSettings { speed = s }) = s

captureInterval :: UAVSettings -> Seconds
captureInterval (UAVSettings { captureInterval = c }) = case c of
  i | i <= 0.0 -> 10.0
  i -> i

shutterSpeed :: UAVSettings -> Int
shutterSpeed (UAVSettings { shutterSpeed = s }) = case s of
  v | v <= 0 -> 1000
  v -> v

pixelWidth :: UAVSettings -> Pixels
pixelWidth (UAVSettings { imageDimensions = dim }) = px dim
  where px (ImageDimensions x _) = x

pixelHeight :: UAVSettings -> Pixels
pixelHeight (UAVSettings { imageDimensions = dim }) = py dim
  where py (ImageDimensions _ y) = y

diagonal35mm :: UAVSettings -> Milimeters
diagonal35mm s = Math.sqrt $ 24.0 * 24.0 + 36.0 * 36.0

diagonalDegrees :: UAVSettings -> Degrees
diagonalDegrees s = 180.0 * 2.0 * (x / Math.pi)
  where
    x = atan $ (diagonal35mm s) / (2.0 * (focalLength s))

diagonalPixels :: UAVSettings -> Meters
diagonalPixels s = Math.sqrt(toNumber $ px * px + py * py)
  where
    px = pixelWidth s
    py = pixelHeight s

diagonalMeters :: UAVSettings -> Meters
diagonalMeters s = alt * tan
  where
    alt = altitude s
    tan = Math.tan(Math.pi * (diagonalDegrees s) / (2.0 * 180.0))

imageIntervalMeters :: UAVSettings -> Meters
imageIntervalMeters s = (speed s) * (captureInterval s)

angularResolution :: UAVSettings -> Number
angularResolution s = (diagonalDegrees s) / (diagonalPixels s)

groundPixelSize :: UAVSettings -> Centimeters
groundPixelSize s = 100.0 * ((diagonalMeters s) / (diagonalPixels s))

motionBlurCentimeters :: UAVSettings -> Centimeters
motionBlurCentimeters s = 100.0 * (speed s) / (toNumber $ shutterSpeed s)

motionBlurPixels :: UAVSettings -> Number
motionBlurPixels s = (100.0 * (speed s) / (toNumber $ shutterSpeed s)) / (groundPixelSize s)

imageOverlapMeters :: UAVSettings -> Meters
imageOverlapMeters s = yMeters - interval
  where
    yMeters  = groundHeight $ footprint s
    interval = imageIntervalMeters s

imageOverlapPercent :: UAVSettings -> Meters
imageOverlapPercent s = calc (imageOverlapMeters s) (groundHeight $ footprint s)
  where
    calc m gh = case m of
      v | v <= 0.0 -> 0.0
      v -> 100.0 * v / gh

fov :: Sensor -> FocalLength -> Fov
fov (Sensor w h) fl = Fov fovx fovy
  where
    fovx = 2.0 * (degrees $ atan (w / (2.0 * fl)))
    fovy = 2.0 * (degrees $ atan (h / (2.0 * fl)))

footprint :: UAVSettings -> Footprint
footprint s = Footprint tall wide
  where
    tall = (toNumber $ pixelHeight s) * (diagonalMeters s) / (diagonalPixels s)
    wide = (toNumber $ pixelWidth s) * (diagonalMeters s) / (diagonalPixels s)

groundHeight :: Footprint -> Meters
groundHeight (Footprint _ h) = h

groundWidth :: Footprint -> Meters
groundWidth (Footprint w _) = w

degrees :: Number -> Number
degrees r = r * (180.0 / Math.pi)
