module Main where

import Control.Monad.Eff.Console
import Prelude
import Coverage

phantomCamera :: Sensor
phantomCamera = Sensor 6.17 4.55

phantomLens :: FocalLength
phantomLens = 5.0

main = do
  display f
  where
    f = fov phantomCamera phantomLens
    display (Fov x y) = log $ "FOV is: { x: " ++ (show x) ++ ", y: " ++ (show y) ++ " }"
