module Main where

import Graphics.Rendering.Cairo hiding (rectangle)
import Graphics.UI.Gtk.Gdk.Events (Event, eventSent)
import Graphics.UI.Gtk hiding (eventSent, rectangle)

import Graphics.Declarative.Backend.Cairo
import Graphics.Declarative.Shape
import Graphics.Declarative.Bordered as Bordered
import qualified Graphics.Declarative.Border as Border
import Data.Vec2 as Vec2

main :: IO ()
main = openFormWindow $ move (50,50) $ debugBoundingBox (solid (1,0,0)) $ alignHoriz 0 $ filled (0,0,0) (circle 20)--append Vec2.right [filled (1, 0.5, 0) $ circle 20, outlined (solid (0, 1, 0)) $ rectangle 42 13]

renderSvgToFile :: Form -> FilePath -> IO ()
renderSvgToFile form path
  = withSVGSurface path (realToFrac w) (realToFrac h) $ \ surface ->
      renderWith surface $ drawForm form
  where
    (w, h) = Border.size . Bordered.getBorder $ form

openFormWindow :: Form -> IO ()
openFormWindow form = openCairoWindow size render
  where
    render = drawForm form
    size   = Border.size . Bordered.getBorder $ form

openCairoWindow :: (Double,Double) -> Render () -> IO ()
openCairoWindow (w,h) render = do
  initGUI
  window <- windowNew
  window `set` windowProperties w h

  canvas <- drawingAreaNew
  containerAdd window canvas

  widgetModifyBg canvas StateNormal white
  widgetShowAll window

  canvas `onExpose` (\ event -> do renderToCanvas render canvas
                                   return (eventSent event))
  window `onDestroy` mainQuit
  mainGUI

renderToCanvas :: WidgetClass w => Render () -> w -> IO ()
renderToCanvas render canvas = do
  (w, h) <- widgetGetSize canvas
  print (w,h)
  drawin <- widgetGetDrawWindow canvas
  renderWithDrawable drawin render

white :: Color
white = Color 65535 65535 65535

windowProperties :: Double -> Double -> [AttrOp Window]
windowProperties width height = [
  windowTitle          := "Cairo Window",
  windowDefaultWidth   := ceiling width,
  windowDefaultHeight  := ceiling height,
  containerBorderWidth := 0 ]
