module Graphics.Declarative.Cairo.Shape where

import qualified Graphics.Rendering.Cairo as Cairo

import Graphics.Declarative.Border as Border
import Graphics.Declarative.Bordered

import Data.Vec2 as Vec2

newtype Shape = Shape (Cairo.Render ())

renderShape :: Shape -> Cairo.Render ()
renderShape (Shape renderer) = renderer

circle :: Double -> Bordered Shape
circle radius = Bordered (Border.circle radius) shape
  where shape = Shape $ Cairo.arc 0 0 radius 0 (2*pi)

rectangle :: Double -> Double -> Bordered Shape
rectangle width height = rectangleFromBB ((-width/2, -height/2), (width/2, height/2))

rectangleFromBB :: (Vec2, Vec2) -> Bordered Shape
rectangleFromBB corners@((l, t), (r, b)) = Bordered (Border.fromBoundingBox corners) shape
  where shape = Shape $ Cairo.rectangle l t (r-l) (b-t)

roundedRectangle :: Double -> Double -> Double -> Bordered Shape
roundedRectangle radius width height = roundedRectangleFromBB radius (Vec2.zero, (width, height))

roundedRectangleFromBB :: Double -> (Vec2, Vec2) -> Bordered Shape
roundedRectangleFromBB radius boundingBox@((left, up), (right, down))
  | radius > width/2 || radius > height/2 = roundedRectangleFromBB (min (width/2) (height/2)) boundingBox
  | otherwise = Bordered hull $ Shape render
  where
    width = right-left
    height = down-up

    innerLeft = left+radius
    innerUp = up+radius
    innerRight = right-radius
    innerDown = down-radius

    hull = Border.padded radius $ Border.fromBoundingBox ((innerLeft, innerUp), (innerRight, innerDown))

    render = do
      Cairo.arc innerLeft  innerUp   radius (Vec2.degrees 180) (Vec2.degrees 270)
      Cairo.arc innerRight innerUp   radius (Vec2.degrees 270) (Vec2.degrees 0)
      Cairo.arc innerRight innerDown radius (Vec2.degrees 0)   (Vec2.degrees 90)
      Cairo.arc innerLeft  innerDown radius (Vec2.degrees 90)  (Vec2.degrees 180)
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
