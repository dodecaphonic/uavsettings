module Main where

import Control.Monad.Eff.Console
import Prelude
import Coverage

phantomCamera :: Sensor
phantomCamera = Sensor 6.17 4.55

phantomLens :: FocalLength
phantomLens = 5.0

settings :: UAVSettings
settings = UAVSettings {
  sensor: phantomCamera
  , focalLength: 30.0
  , imageDimensions: ImageDimensions 4384 3288
  , speed: 3.0
  , captureInterval: 3.0
  , shutterSpeed: 250
  , gimbalX: 30.0
  , gimbalY: 30.0
  , groundAltitude: 100.0
}

main = do
  display
  where
    f = fov phantomCamera phantomLens
    gpx = groundPixelSize settings
    display = log $ "Ground pixel size is " ++ (show gpx) ++ "\n" ++
      "Image overlap (m) is " ++ (show $ imageOverlapMeters settings) ++ "\n" ++
      "Image overlap (%) is " ++ (show $ imageOverlapPercent settings) ++ "\n" ++
      "Image interval (m) is " ++ (show $ imageIntervalMeters settings) ++ "\n" ++
      "Image x size is " ++ (show $ groundHeight $ footprint settings) ++ "\n" ++
      "Image y size is " ++ (show $ groundWidth $ footprint settings) ++ "\n" ++
      "Motion blur (cm) is " ++ (show $ motionBlurCentimeters settings) ++ "\n" ++
      "Motion blur (px) is " ++ (show $ motionBlurPixels settings) ++ "\n"
