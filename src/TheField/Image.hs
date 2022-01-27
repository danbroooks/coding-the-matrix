module TheField.Image where

import qualified Codec.Picture as JP

intensity :: JP.PixelRGB8 -> Rational
intensity (JP.PixelRGB8 r g b) =
  255 - (0.2126 * toRational r) - (0.7152 * toRational g) - (0.0722 * toRational b)

pixels :: JP.Image JP.PixelRGB8 -> [(Int, Int, JP.PixelRGB8)]
pixels img =
  [ (x, h - y, JP.pixelAt img x y) | x <- [0..w-1], y <- [0..h-1] ]
  where
    w =
      JP.imageWidth img
    h =
      JP.imageHeight img

readImage :: FilePath -> IO (JP.Image JP.PixelRGB8)
readImage path = do
  loadResult <- fmap JP.convertRGB8 <$> JP.readImage path
  case loadResult of
    Right img -> pure img
    Left err -> fail err

displayDynamicImageFormat :: JP.DynamicImage -> String
displayDynamicImageFormat img =
  case img of
    JP.ImageY8 _ -> "y8"
    JP.ImageY16 _ -> "y16"
    JP.ImageY32 _ -> "y32"
    JP.ImageYF _ -> "yf"
    JP.ImageYA8 _ -> "ya8"
    JP.ImageYA16 _ -> "ya16"
    JP.ImageRGB8 _ -> "rgb8"
    JP.ImageRGB16 _ -> "rgb16"
    JP.ImageRGBF _ -> "rgbf"
    JP.ImageRGBA8 _ -> "rgba8"
    JP.ImageRGBA16 _ -> "rgba16"
    JP.ImageYCbCr8 _ -> "ycbcr8"
    JP.ImageCMYK8 _ -> "cmyk8"
    JP.ImageCMYK16 _ -> "cmyk16"
