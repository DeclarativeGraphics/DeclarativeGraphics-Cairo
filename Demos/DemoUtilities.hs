module Demos.DemoUtilities where

import Graphics.UI.Gtk hiding (eventSent)
import Graphics.UI.Gtk.Gdk.Events (Event, eventSent)
import Graphics.Rendering.Cairo hiding (rectangle)
import Graphics.Declarative.Graphic
import Graphics.Declarative.Envelope
import Graphics.Declarative.Backend.Cairo

makeSvg :: CairoGraphic -> FilePath -> IO ()
makeSvg graphic path = withSVGSurface path (realToFrac w) (realToFrac h) (\surface -> renderWith surface (drawGraphic graphic w h))
  where
    (ws, hs) = sizeEnvelope $ getEnvelope graphic
    w = ws + 10
    h = hs + 10

openGTK :: CairoGraphic -> IO ()
openGTK graphic = do
  let (w, h) = sizeEnvelope $ getEnvelope graphic
  initGUI
  window <- windowNew
  set window (windowProperties (w+10) (h+10))

  canvas <- drawingAreaNew
  containerAdd window canvas

  widgetModifyBg canvas StateNormal white
  widgetShowAll window

  onExpose canvas (handleExpose graphic canvas)
  onDestroy window mainQuit
  mainGUI

handleExpose :: WidgetClass w => CairoGraphic -> w -> Event -> IO Bool
handleExpose graphic canvas event = do
  (w, h) <- widgetGetSize canvas
  drawin <- widgetGetDrawWindow canvas
  renderWithDrawable drawin (drawGraphic graphic (fromIntegral w) (fromIntegral h))
  return (eventSent event)

white :: Color
white = Color 65535 65535 65535

windowProperties :: Double -> Double -> [AttrOp Window]
windowProperties width height = [
  windowTitle          := "Hello Cairo",
  windowDefaultWidth   := ceiling width,
  windowDefaultHeight  := ceiling height,
  containerBorderWidth := 0 ]

drawGraphic :: CairoGraphic -> Double -> Double -> Render ()
drawGraphic graphic w h = drawCairo $ move (w / 2, h / 2) $ centered $ graphic
