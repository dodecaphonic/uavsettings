# uavsettings

_uavsettings_ is a PureScript library for pinpointing the final characteristics of an aerial photograph taken by a drone (or, more technically, an UAV). Its goal is to help in fine-tuning a flight's photography by letting a user change settings and see what impact it will have in the photogrammetric products.

## Usage

You must first define a UAVSettings object, such as:

```purescript
import Coverage

phantomCamera :: Sensor
phantomCamera = { width: 6.17, height: 4.55 }

phantomLens :: FocalLength
phantomLens = 5.0

phantomImageDimensions :: ImageDimensions
phantomImageDimensions = { width: 4384, height: 3288 }

settings :: UAVSettings
settings = {
  sensor: phantomCamera
  , focalLength: phantomLens 
  , imageDimensions: phantomImageDimensions
  , speed: 3.0 :: MetersPerSecond
  , captureInterval: 3.0 :: Seconds
  , shutterSpeed: 250 :: Int
  , gimbalX: 30.0 :: Degrees
  , gimbalY: 30.0 :: Degrees
  , groundAltitude: 100.0 :: Meters
}
```

Then you can pick and choose from the functions in the `Coverage` module to find out what photogrammetric characteristics those settings would yield, such as:

``` purescript
> Coverage.imageOverlapMeters settings
52.33000570683162

> Coverage.groundPixelSize settings
1.3989508601010863
```

There's an example in `src/Main.purs`. Run it with `pulp run`.

## Testing

Tests are implemented as [purescript-quickcheck][quickcheck] properties. Run them with `pulp test`.


[quickcheck]: https://github.com/purescript/purescript-quickcheck
