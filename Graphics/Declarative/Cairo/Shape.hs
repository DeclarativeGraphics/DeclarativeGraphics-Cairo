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
rectangle width height = bordered (0.5,0.5) width height shape
  where shape = Shape $ Cairo.rectangle (-width/2) (-height/2) width height

roundedRectangle :: Double -> Double -> Double -> Bordered Shape
roundedRectangle width height radius
  | radius > width/2 || radius > height/2 = roundedRectangle width height $ min (width/2) (height/2)
  | otherwise = Bordered hull $ Shape render
  where
    hull = Border.padded radius $ Border.fromBoundingBox (Vec2.zero, (width-radius, height-radius))
    render = do
      Cairo.arc radius         radius          radius (Vec2.degrees 180) (Vec2.degrees 270)
      Cairo.arc (width-radius) radius          radius (Vec2.degrees 270) (Vec2.degrees 0)
      Cairo.arc (width-radius) (height-radius) radius (Vec2.degrees 0)   (Vec2.degrees 90)
      Cairo.arc radius         (height-radius) radius (Vec2.degrees 90)  (Vec2.degrees 180)


fromBoundingBox :: (Vec2, Vec2) -> Bordered Shape
fromBoundingBox corners@((l, t), (r, b)) = Bordered (Border.fromBoundingBox corners) shape
  where shape = Shape $ Cairo.rectangle l t (r-l) (b-t)


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
