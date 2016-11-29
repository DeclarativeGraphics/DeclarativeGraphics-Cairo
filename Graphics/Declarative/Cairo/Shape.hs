module Graphics.Declarative.Cairo.Shape where

import qualified Graphics.Rendering.Cairo as Cairo

import Graphics.Declarative.Border as Border
import Graphics.Declarative.Bordered

import Linear

newtype Shape = Shape (Cairo.Render ())

renderShape :: Shape -> Cairo.Render ()
renderShape (Shape renderer) = renderer

circle :: Double -> Bordered Shape
circle radius = Bordered (Border.circle radius) shape
  where shape = Shape $ Cairo.arc 0 0 radius 0 (2*pi)

rectangle :: Double -> Double -> Bordered Shape
rectangle width height = rectangleFromBB (V2 (-width/2) (-height/2), V2 (width/2) (height/2))

rectangleFromBB :: (V2 Double, V2 Double) -> Bordered Shape
rectangleFromBB corners@(V2 l t, V2 r b) = Bordered (Border.fromBoundingBox corners) shape
  where shape = Shape $ Cairo.rectangle l t (r-l) (b-t)

roundedRectangle :: Double -> Double -> Double -> Bordered Shape
roundedRectangle radius width height = roundedRectangleFromBB radius (V2 0 0, V2 width height)

roundedRectangleFromBB :: Double -> (V2 Double, V2 Double) -> Bordered Shape
roundedRectangleFromBB radius boundingBox@(V2 left up, V2 right down)
  | radius > width/2 || radius > height/2 = roundedRectangleFromBB (min (width/2) (height/2)) boundingBox
  | otherwise = Bordered hull $ Shape render
  where
    width = right-left
    height = down-up

    innerLeft = left+radius
    innerUp = up+radius
    innerRight = right-radius
    innerDown = down-radius

    hull = Border.padded radius $ Border.fromBoundingBox (V2 innerLeft innerUp, V2 innerRight innerDown)

    degrees = (*) (pi / 180)

    render = do
      Cairo.arc innerLeft  innerUp   radius (degrees 180) (degrees 270)
      Cairo.arc innerRight innerUp   radius (degrees 270) (degrees 0)
      Cairo.arc innerRight innerDown radius (degrees 0)   (degrees 90)
      Cairo.arc innerLeft  innerDown radius (degrees 90)  (degrees 180)
      Cairo.closePath


data Path = Path {
  pathStart    :: (Double,Double),
  pathRenderer :: Cairo.Render ()
}

renderOpenPath :: Path -> Cairo.Render ()
renderOpenPath (Path start renderer) = uncurry Cairo.moveTo start >> renderer

renderClosedPath :: Path -> Cairo.Render ()
renderClosedPath path = renderOpenPath path >> Cairo.closePath


openPath :: Path -> Shape
openPath = Shape . renderOpenPath

closedPath :: Path -> Shape
closedPath = Shape . renderClosedPath


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
