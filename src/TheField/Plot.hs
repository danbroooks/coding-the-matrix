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
  layout_x_axis . laxis_generate .= scaledAxis def (-w,w)
  layout_y_axis . laxis_generate .= scaledAxis def (-h,h)
  layout_bottom_axis_visibility . axis_show_line .= False
  layout_bottom_axis_visibility . axis_show_ticks .= False
  layout_left_axis_visibility . axis_show_line .= False
  layout_left_axis_visibility . axis_show_ticks .= False
  layout_legend .= Nothing
  setColors [opaque red, opaque black, opaque black]
  plot (points "s" $ L.map plotable s)
  plot (line "x" [x])
  plot (line "y" [y])
    where
      x = [(-w*2, 0), (w*2, 0)]
      y = [(0, -h*2), (0, h*2)]

