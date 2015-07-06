module Coverage where

import Math
import Prelude

type Milimiters = Number
type Meters = Number
type Degrees = Number
type FocalLength = Milimiters

data Sensor = Sensor Milimiters Milimiters

data UAVSettings = UAVSettings {
  sensor :: Sensor
  , focalLength :: FocalLength
  , gimbalX :: Number
  , gimbalY :: Number
  , groundAltitude :: Meters
}

data Footprint = Footprint Meters Meters

data Fov = Fov Degrees Degrees

settingsSensor :: UAVSettings -> Sensor
settingsSensor (UAVSettings { sensor = s }) = s

settingsLens :: UAVSettings -> FocalLength
settingsLens (UAVSettings { focalLength = l }) = l

settingsAltitude :: UAVSettings -> Meters
settingsAltitude (UAVSettings { altitude = a }) = a

fov :: Sensor -> FocalLength -> Fov
fov (Sensor w h) fl = Fov fovx fovy
  where
    fovx = 2.0 * (degrees $ atan (w / (2.0 * fl)))
    fovy = 2.0 * (degrees $ atan (h / (2.0 * fl)))

footprint :: UAVSettings -> Footprint
footprint s = Footprint tall wide
  where
    view = fov (settingsSensor s) (settingsLens s)
    tall = (settingsAltitude s) *
    wide = 2

degrees :: Number -> Number
degrees r = r * (180.0 / Math.pi)
