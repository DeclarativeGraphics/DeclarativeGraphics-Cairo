module Graphics.Declarative.Cairo.Utils where

import Graphics.Rendering.Cairo
import Graphics.Declarative.Cairo.Form

writeAsSvgToFile :: Double -> Double -> Form -> FilePath -> IO ()
writeAsSvgToFile width height form file
  = withSVGSurface file width height $ \ surface ->
      renderWith surface $ drawForm form
