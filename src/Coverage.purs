module Coverage where

import Math
import Prelude

type Milimiters = Number
type Degrees = Number

type Camera = {
  name :: String
  , sensorWidth :: Milimiters
  , sensorHeight :: Milimiters
}

type CameraLens = {
  focalLength :: Milimiters
}

type Settings = {
  camera :: Camera
  , lens :: CameraLens
  , gimbalX :: Number
  , gimbalY :: Number
  , altitude :: Number
}

type Fov = { x :: Degrees, y :: Degrees }

fov :: Camera -> CameraLens -> Fov
fov camera lens = { x: fovx, y: fovy } :: Fov
  where
    fovx = 2.0 * (degrees (atan (camera.sensorWidth / (2.0 * lens.focalLength))))
    fovy = 2.0 * (degrees (atan (camera.sensorHeight / (2.0 * lens.focalLength))))

degrees :: Number -> Number
degrees r = r * (180.0 / Math.pi)

test = let
  camera = { name: "Foo", sensorWidth: 36.0, sensorHeight: 24.0 } :: Camera
  lens = { focalLength: 20.0 }
  in fov camera lens
