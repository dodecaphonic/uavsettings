module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Prelude (Unit, show, ($), (++))
import UAVSettings.Coverage
import UAVSettings.Units (FocalLength, Meters(Meters))

phantomCamera :: Sensor
phantomCamera = { width: 6.17, height: 4.55 }

phantomLens :: FocalLength
phantomLens = 5.0

fullFrame :: Sensor
fullFrame = { width: 36.0, height: 24.0 }

wideAngle :: FocalLength
wideAngle = 30.0

settings :: UAVSettings
settings = {
  sensor: phantomCamera
  , focalLength: phantomLens
  , imageDimensions: { width: 4384.0, height: 3288.0 }
  , speed: 3.0
  , captureInterval: 3.0
  , shutterSpeed: 1000
  , gimbalX: 30.0
  , gimbalY: 30.0
  , groundAltitude: Meters 80.0
}

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  log $ "Ground pixel size is " ++ (show $ groundPixelSize settings) ++ "\n" ++
        "Image overlap (m) is " ++ (show $ imageOverlapMeters settings) ++ "\n" ++
        "Image overlap (%) is " ++ (show $ imageOverlapPercent settings) ++ "\n" ++
        "Image interval (m) is " ++ (show $ imageIntervalMeters settings) ++ "\n" ++
        "Image x size is " ++ (show $ (footprint settings).height) ++ "\n" ++
        "Image y size is " ++ (show $ (footprint settings).width) ++ "\n" ++
        "Motion blur (cm) is " ++ (show $ motionBlurCentimeters settings) ++ "\n" ++
        "Motion blur (px) is " ++ (show $ motionBlurPixels settings) ++ "\n" ++
        "FOV wide (m) is " ++ (show $ fov.x) ++ "\n" ++
        "FOV tall (m) is " ++ (show $ fov.y) ++ "\n"
  where
    fov = fieldOfView settings
