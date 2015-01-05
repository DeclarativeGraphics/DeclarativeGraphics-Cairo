module Graphics.Declarative.Cairo.Debug where

import           Graphics.Declarative.Bordered  as Bordered
import qualified Graphics.Declarative.Border    as Border

import           Graphics.Declarative.Cairo.Shape as Shape
import           Graphics.Declarative.Cairo.Form  as Form

import Data.Vec2 as Vec2


boundingBoxShape :: Form -> Bordered Shape
boundingBoxShape = fromBoundingBox . Border.getBoundingBox . Bordered.getBorder

debugBoundingBox :: LineStyle -> Form -> Form
debugBoundingBox linestyle graphic
  = foldr1 Bordered.atop [collapseBorder origincircle, collapseBorder boundingbox, graphic]
  where
    origincircle = outlined linestyle $ circle 2
    boundingbox  = outlined linestyle $ boundingBoxShape graphic

debugWithSize :: Form -> Form
debugWithSize graphic
  = collapseBorder (Bordered.move (Vec2.add left bottom) debugText)
    `Bordered.atop`
    debugBoundingBox (solid (1,0,0)) graphic
  where
    debugText = text monoStyle $ unlines ["width : " ++ show w
                                         ,"height: " ++ show h]
    left   = Border.borderOffset (getBorder graphic) Vec2.left
    bottom = Border.borderOffset (getBorder graphic) Vec2.down
    w = graphicWidth graphic
    h = graphicHeight graphic
    monoStyle = defaultTextStyle { fontFamily = "Monospace", fontSize = 8 }

tangentPath :: Double -> Vec2 -> Path
tangentPath size tangentPoint
  = pathPoint (toLeft `Vec2.add` tangentPoint) `lineConnect` pathPoint (toRight `Vec2.add` tangentPoint)
  where
    normalVector = Vec2.normalize tangentPoint
    toLeft  = Vec2.scale size . Vec2.rotateBy (degrees    90) $ normalVector
    toRight = Vec2.scale size . Vec2.rotateBy (degrees (-90)) $ normalVector

vectorPath :: LineStyle -> Vec2 -> Path
vectorPath linestyle vector = pathPoint (0,0) `lineConnect` pathPoint vector
