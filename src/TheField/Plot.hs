{-# LANGUAGE FlexibleInstances #-}

module TheField.Plot where

import Data.Complex
import Data.List as L
import Graphics.Rendering.Chart.Easy

class Plotable a where
  plotable :: RealFloat b => a -> (b, b)

instance RealFloat a => Plotable (Complex a) where
  plotable c = (realToFrac . realPart $ c, realToFrac . imagPart $ c)

instance RealFloat a => Plotable (a, a) where
  plotable (a, b) = (realToFrac a, realToFrac b)

plot' :: (Plotable a, RealFloat b, Show b) => [a] -> b -> b -> EC (Layout b b) ()
plot' s w h = do
  setColors [opaque red]
  plot (points "s" $ L.map plotable s)
  vectorPlot w h

plotComplexAddition :: Complex Double -> Complex Double -> EC (Layout Double Double) ()
plotComplexAddition a b = do
  vectorPlot (maxReal * 1.1) (maxImag * 1.1)
  plot . liftEC $ do
    plot_lines_style . line_color .= opaque red
    plot_lines_style . line_width .= 3
    plot_lines_values .= L.map (L.map plotable) [[0, a], [a, a + b]]
  plot . liftEC $ do
    plot_lines_style . line_color .= opaque green
    plot_lines_style . line_width .= 2
    plot_lines_style . line_dashes .= [5, 5]
    plot_lines_values .= [ L.map plotable [0, a + b] ]
  where
    maxReal = maximum [ realPart a, realPart b, realPart (a + b) ]
    maxImag = maximum [ imagPart a, imagPart b, imagPart (a + b) ]

vectorPlot :: (RealFloat a, Show a) => a -> a -> EC (Layout a a) ()
vectorPlot w h = do
  layout_x_axis . laxis_generate .= scaledAxis def (-w,w)
  layout_y_axis . laxis_generate .= scaledAxis def (-h,h)
  layout_bottom_axis_visibility . axis_show_line .= False
  layout_bottom_axis_visibility . axis_show_ticks .= False
  layout_left_axis_visibility . axis_show_line .= False
  layout_left_axis_visibility . axis_show_ticks .= False
  layout_legend .= Nothing
  setColors [opaque black, opaque black]
  plot (line "x" [x])
  plot (line "y" [y])
    where
      x = [(-w*2, 0), (w*2, 0)]
      y = [(0, -h*2), (0, h*2)]

