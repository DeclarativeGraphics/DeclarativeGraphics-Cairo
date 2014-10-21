module Graphics.Declarative.Backend.Cairo where

import System.IO.Unsafe (unsafePerformIO)
import Graphics.Rendering.Pango hiding (Color, Font)

import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Declarative.Internal.Primitive as Prim

import Graphics.Declarative.Graphic
import Graphics.Declarative.Envelope
import Graphics.Declarative.Enveloped
import Graphics.Declarative.Shape

type CairoEGraphic = Enveloped CairoGraphic

type CairoGraphic = Graphic (Cairo.Render ())

data LineCap = Flat | Round | Padded deriving (Show, Eq)
data LineJoin = Clipped | Smooth | Sharp deriving (Show, Eq)

data LineStyle = LineStyle {
  color :: RGB,
  lineWidth :: Double,
  cap :: LineCap,
  lineJoin :: LineJoin,
  dash :: [Double],
  dashOffset :: Double
} deriving (Show, Eq)

defaultLineStyle :: LineStyle
defaultLineStyle = LineStyle {
  color = (0, 0, 0),
  lineWidth = 1,
  cap = Padded,
  lineJoin = Sharp,
  dash = [],
  dashOffset = 0
}

data TextStyle = TextStyle {
  textColor :: RGB,
  bold :: Bool,
  italic :: Bool,
  fontSize :: Double,
  fontFamily :: String
} deriving (Show, Eq)

defaultTextStyle :: TextStyle
defaultTextStyle = TextStyle {
  textColor = (0, 0, 0),
  bold = False,
  italic = False,
  fontSize = 14,
  fontFamily = "Sans"
}

drawCairo :: CairoGraphic -> Cairo.Render ()
drawCairo (Leaf rend) = rend
drawCairo (Atop g1 g2) = drawCairo g2 >> drawCairo g1
drawCairo Empty = return ()
drawCairo (Moved dist graphic) = do
  Cairo.save
  uncurry Cairo.translate dist
  drawCairo graphic
  Cairo.restore

drawEnvelopedCairo :: CairoEGraphic -> Cairo.Render ()
drawEnvelopedCairo (Pack _ g) = drawCairo g

filled :: RGB -> Shape -> CairoEGraphic
filled (r, g, b) shape = Pack (sEnvelope shape) $ Leaf $ do
    Cairo.save
    Cairo.setSourceRGB r g b
    Prim.renderPrimitives $ sPrims shape
    Cairo.fill
    Cairo.restore

outlined :: LineStyle -> Shape -> CairoEGraphic
outlined style shape = Pack (sEnvelope shape) $ Leaf $ do
  Cairo.save
  applyLineStyle style
  Prim.renderPrimitives $ sPrims shape
  Cairo.stroke
  Cairo.restore

solid :: RGB -> LineStyle
solid col = defaultLineStyle { color = col }

applyLineStyle :: LineStyle -> Cairo.Render ()
applyLineStyle style = do
  Cairo.setSourceRGB r g b
  Cairo.setLineWidth $ lineWidth style
  Cairo.setLineCap $ convertLineCap $ cap style
  Cairo.setLineJoin $ convertLineJoin $ lineJoin style
  Cairo.setDash (dash style) (dashOffset style)
  where
    (r, g, b) = color style

convertLineCap :: LineCap -> Cairo.LineCap
convertLineCap Flat = Cairo.LineCapButt
convertLineCap Round = Cairo.LineCapRound
convertLineCap Padded = Cairo.LineCapSquare

convertLineJoin :: LineJoin -> Cairo.LineJoin
convertLineJoin Clipped = Cairo.LineJoinBevel
convertLineJoin Smooth = Cairo.LineJoinRound
convertLineJoin Sharp = Cairo.LineJoinMiter

text :: TextStyle -> String -> CairoEGraphic
text style content = Pack (Envelope 0 0 width height) $ Leaf $ do
  Cairo.save
  Cairo.setSourceRGB r g b
  showLayout pLayout
  Cairo.restore
    where
      (_, PangoRectangle _ _ width height) = unsafePerformIO $ layoutGetExtents pLayout
      (r, g, b) = textColor style
      getFontDescription :: IO FontDescription
      getFontDescription = do
        desc <- fontDescriptionNew
        fontDescriptionSetStyle desc $
          if italic style then StyleItalic else StyleNormal
        fontDescriptionSetWeight desc $
          if bold style then WeightBold else WeightNormal
        fontDescriptionSetSize desc $ fontSize style
        fontDescriptionSetFamily desc $ fontFamily style
        return desc
      getContext :: FontDescription -> IO PangoContext
      getContext fontDescription = do
        context <- cairoCreateContext Nothing
        contextSetFontDescription context fontDescription
        return context
      pLayout :: PangoLayout
      pLayout = unsafePerformIO $ do
        pContext <- getContext =<< getFontDescription
        layoutText pContext content

debugEnvelope :: CairoEGraphic -> CairoEGraphic
debugEnvelope graphic =
  noEnvelope (outlined (solid (1, 0, 0)) $ circle 2)
  `atop`
  outlined (solid (1, 0, 0)) (fromEnvelope $ getEnvelope graphic)
  `atop`
  graphic

debugWithSize :: CairoEGraphic -> CairoEGraphic
debugWithSize graphic =
  noEnvelope (move (left, bottom) $ text monoStyle $ "width : " ++ show w ++ "\nheight: " ++ show h)
  `atop`
  debugEnvelope graphic
  where
    (Envelope left _ _ bottom) = getEnvelope graphic
    w = graphicWidth graphic
    h = graphicHeight graphic
    monoStyle = defaultTextStyle { fontFamily = "Monospace", fontSize = 8 }
