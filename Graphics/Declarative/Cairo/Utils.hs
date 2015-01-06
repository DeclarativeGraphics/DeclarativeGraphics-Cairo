module Graphics.Declarative.Cairo.Utils where

import Graphics.Declarative.Cairo.Form

writeAsSvgToFile :: Int -> Int -> Form -> FilePath -> IO ()
writeAsSvgToFile width height form file
  = withSVGSurface path width height $ \ surface ->
      renderWith surface $ drawForm form
