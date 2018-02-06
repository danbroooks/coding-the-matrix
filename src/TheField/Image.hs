module TheField.Image where

import           Vision.Image
import           Vision.Image.Storage.DevIL (Autodetect (..), load)
import           Vision.Primitive (Z(..), (:.)(..), ix2)

intensity :: RGBPixel -> Rational
intensity (RGBPixel r g b) =
  255 - (0.2126 * toRational r) - (0.7152 * toRational g) - (0.0722 * toRational b)

pixels :: Manifest RGBPixel -> [(Int, Int, RGBPixel)]
pixels m =
  [ (x, h - y, index m (ix2 y x)) | x <- [0..w-1], y <- [0..h-1] ]
  where
    (Z :. h :. w) = manifestSize m

readImage :: FilePath -> IO RGB
readImage path = load Autodetect path >>= either (fail . show) return

