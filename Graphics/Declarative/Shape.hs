module Graphics.Declarative.Shape where

import qualified Graphics.Rendering.Cairo as Cairo

import Graphics.Declarative.Frame
import Graphics.Declarative.Framed

newtype Shape = Shape (Cairo.Render ())

renderShape :: Shape -> Cairo.Render ()
renderShape (Shape renderer) = renderer

circle :: Double -> Framed Shape
circle radius = framed (0.5,0.5) (2*radius) (2*radius) shape
  where shape = Shape $ Cairo.arc 0 0 radius 0 (2*pi)

rectangle :: Double -> Double -> Framed Shape
rectangle width height = framed (0.5,0.5) width height shape
  where shape = Shape $ Cairo.rectangle (-width/2) (-height/2) width height


empty :: Framed Shape
empty = gap 0 0

gap :: Double -> Double -> Framed Shape
gap w h = framed (0.5,0.5) w h (Shape (return ()))

fromFrame :: Frame -> Framed Shape
fromFrame frame@(Frame l t r b) = Framed frame shape
  where shape = Shape $ Cairo.rectangle l t (r-l) (b-t)


data Path = Path {
  pathStart    :: (Double,Double),
  pathRenderer :: Cairo.Render ()
}

pathToShape :: Path -> Framed Shape
pathToShape = noFrame . Shape . pathRenderer

pathPoint :: (Double,Double) -> Path
pathPoint point = Path point (return ())

connectBy :: (Double -> Double -> Cairo.Render ())
          -> Path -> Path -> Path
connectBy connector (Path start0 prim0) (Path start1 prim1)
  = Path start0 (prim0 >> connection >> prim1)
  where
    connection = uncurry connector start1

lineConnect :: Path -> Path -> Path
lineConnect = connectBy Cairo.lineTo

curveConnect :: (Double,Double) -> (Double,Double)
             -> Path -> Path -> Path
curveConnect (x1,y1) (x2,y2) = connectBy (Cairo.curveTo x1 y1 x2 y2)
