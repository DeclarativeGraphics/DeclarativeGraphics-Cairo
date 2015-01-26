module Graphics.Declarative.Cairo.Debug where

import qualified Graphics.Declarative.Bordered  as Bordered  hiding (map)
import qualified Graphics.Declarative.Border    as Border
import           Graphics.Declarative.Bordered (Bordered)
import           Graphics.Declarative.Border   (Border)
import           Graphics.Declarative.Graphic
import           Graphics.Declarative.Physical2D

import           Graphics.Declarative.Cairo.Shape as Shape
import           Graphics.Declarative.Cairo.Form  as Form
import           Graphics.Declarative.Cairo.TangoColors as Colors

import qualified Data.Vec2 as Vec2
import Data.Vec2 (Vec2)

data DebuggedForm = Debugged { debugInfo :: Form, debugOriginal :: Form }

instance Physical2D DebuggedForm where
  move offset   = onDebug $ move offset
  rotate angle  = onDebug $ rotate angle
  scale factors = onDebug $ scale factors
  atop          = onDebug2 atop
  empty         = noDebug empty

debuggedForm :: DebuggedForm -> Form
debuggedForm (Debugged info original) = info `atop` original

noDebug :: Form -> DebuggedForm
noDebug = Debugged empty

attachDebugInfo :: Form -> DebuggedForm -> DebuggedForm
attachDebugInfo info = onDebugInfo (atop info)

createDebugInfo :: (Form -> Form) -> DebuggedForm -> DebuggedForm
createDebugInfo createDebugInfo debugform
  = attachDebugInfo newDebugInfo debugform
  where
    newDebugInfo = createDebugInfo (debugOriginal debugform)

onDebugInfo :: (Form -> Form) -> DebuggedForm -> DebuggedForm
onDebugInfo f (Debugged info original)
  = Debugged (f info) original

onOriginal :: (Form -> Form) -> DebuggedForm -> DebuggedForm
onOriginal f (Debugged info original)
  = Debugged info (f original)

onDebug :: (Form -> Form) -> DebuggedForm -> DebuggedForm
onDebug f (Debugged info original) = Debugged (f info) (f original)

onDebug2 :: (Form -> Form -> Form)
         -> (DebuggedForm -> DebuggedForm -> DebuggedForm)
onDebug2 f (Debugged infoL originalL) (Debugged infoR originalR)
  = Debugged (f infoL infoR)
             (f originalL originalR)

boundingBoxShape :: Form -> Bordered Shape
boundingBoxShape
  = Shape.rectangleFromBB . Border.getBoundingBox . Bordered.getBorder

createBoundingBox :: LineStyle -> Form -> Form
createBoundingBox linestyle form = origincircle `atop` boundingbox
  where
    origincircle = outlined linestyle $ circle 2
    boundingbox  = outlined linestyle $ boundingBoxShape form

debugBoundingBox :: LineStyle -> DebuggedForm -> DebuggedForm
debugBoundingBox linestyle = createDebugInfo (createBoundingBox linestyle)

-- use DebuggedForm instead of Form
debugWithSize :: Form -> Form
debugWithSize graphic
  = Bordered.collapseBorder (move (Vec2.add left bottom) debugText)
    `atop`
    debuggedForm (debugBoundingBox (solid (1,0,0)) (noDebug graphic))
  where
    debugText = text monoStyle $ unlines ["width : " ++ show w
                                         ,"height: " ++ show h]
    left   = Border.borderOffset (Bordered.getBorder graphic) Vec2.left
    bottom = Border.borderOffset (Bordered.getBorder graphic) Vec2.down
    w = Bordered.graphicWidth graphic
    h = Bordered.graphicHeight graphic
    monoStyle = defaultTextStyle { fontFamily = "Monospace", fontSize = 8 }

tangentPath :: Double -> Vec2 -> Path
tangentPath size tangentPoint
  = pathPoint (toLeft `Vec2.add` tangentPoint) `lineConnect` pathPoint (toRight `Vec2.add` tangentPoint)
  where
    normalVector = Vec2.normalize tangentPoint
    toLeft  = Vec2.scale size . Vec2.rotateBy (Vec2.degrees    90) $ normalVector
    toRight = Vec2.scale size . Vec2.rotateBy (Vec2.degrees (-90)) $ normalVector

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

createTangent :: Vec2 -> Form -> Form
createTangent direction form = outlined (solid red) tangentShape
  where
    tangentShape = Bordered.noBorder . openPath $ tangentPath 100 borderVector
    borderVector = Border.borderOffset (Bordered.getBorder form) direction

debugTangent :: Vec2 -> DebuggedForm -> DebuggedForm
debugTangent direction = createDebugInfo (createTangent direction)

createTangentVector :: Vec2 -> Form -> Form
createTangentVector direction form = outlined (solid red) tangentVectorShape
  where
    tangentVectorShape = Bordered.noBorder . openPath $ vectorPath tangentVector
    tangentVector      = Border.borderOffset (Bordered.getBorder form) direction

debugTangentVector :: Vec2 -> DebuggedForm -> DebuggedForm
debugTangentVector direction = createDebugInfo (createTangentVector direction)

createTangentHull :: Double -> Form -> Form
createTangentHull step form = outlined (solid red) hullshape
  where
    hullshape = Bordered.noBorder . closedPath $ hullpath
    hullpath  = tangentHullPath step (Bordered.getBorder form)

debugTangentHull :: Double -> DebuggedForm -> DebuggedForm
debugTangentHull step = createDebugInfo (createTangentHull step)
