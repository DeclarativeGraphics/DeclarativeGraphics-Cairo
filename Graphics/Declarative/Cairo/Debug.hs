module Graphics.Declarative.Cairo.Debug where

import           Graphics.Declarative.Bordered  as Bordered  hiding (map)
import qualified Graphics.Declarative.Border    as Border
import           Graphics.Declarative.Border (Border)

import           Graphics.Declarative.Cairo.Shape as Shape
import           Graphics.Declarative.Cairo.Form  as Form
import           Graphics.Declarative.Cairo.TangoColors as Colors

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

vectorPath :: Vec2 -> Path
vectorPath vector = pathPoint (0,0) `lineConnect` pathPoint vector

vectorsInCircle :: Double -> [Vec2]
vectorsInCircle step = map getVector [0,step..2*pi]
  where
    getVector dir = Vec2.rotateBy dir Vec2.right

tangentHullPoints :: Double -> Border -> [Vec2]
tangentHullPoints step border
  = map (Border.borderOffset border) $ vectorsInCircle step

tangentHullPath :: Double -> Border -> Path
tangentHullPath step = foldr1 lineConnect . map pathPoint . tangentHullPoints step

debugTangent :: Vec2 -> Form -> Form
debugTangent direction form = tangent `atop` form
  where
    tangent = outlined (solid red) $ noBorder . openPath $ tangentPath 100 borderVector
    borderVector = Border.borderOffset (Bordered.getBorder form) direction

debugTangentHull :: Double -> Form -> Form
debugTangentHull step form = hull `atop` form
  where
    hull      = outlined (solid red) hullshape
    hullshape = noBorder . closedPath $ hullpath
    hullpath  = tangentHullPath step (Bordered.getBorder form)
