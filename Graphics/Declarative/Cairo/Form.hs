module Graphics.Declarative.Cairo.Form where


import System.IO.Unsafe (unsafePerformIO)

import           Graphics.Rendering.Pango hiding (Color, Font)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Matrix as Cairo

import           Graphics.Declarative.Graphic  as Graphic
import           Graphics.Declarative.Bordered as Bordered
import qualified Graphics.Declarative.Border   as Border
import           Graphics.Declarative.Classes

import           Graphics.Declarative.Cairo.Shape as Shape

import Linear

type RGB = (Double, Double, Double)

type Form = Bordered (Graphic (Cairo.Render ()))

data LineCap = Flat | Round | Padded deriving (Show, Eq)
data LineJoin = Clipped | Smooth | Sharp deriving (Show, Eq)

data LineStyle = LineStyle {
  color      :: RGB,
  lineWidth  :: Double,
  cap        :: LineCap,
  lineJoin   :: LineJoin,
  dash       :: [Double],
  dashOffset :: Double
} deriving (Show, Eq)

defaultLineStyle :: LineStyle
defaultLineStyle = LineStyle {
  color      = (0, 0, 0),
  lineWidth  = 1,
  cap        = Padded,
  lineJoin   = Sharp,
  dash       = [],
  dashOffset = 0
}

solid :: RGB -> LineStyle
solid col = defaultLineStyle { color = col }

data TextStyle = TextStyle {
  textColor  :: RGB,
  bold       :: Bool,
  italic     :: Bool,
  fontSize   :: Double,
  fontFamily :: String
} deriving (Show, Eq)

defaultTextStyle :: TextStyle
defaultTextStyle = TextStyle {
  textColor  = (0, 0, 0),
  bold       = False,
  italic     = False,
  fontSize   = 14,
  fontFamily = "Sans"
}

font :: String -> Double -> TextStyle
font family size = defaultTextStyle { fontFamily = family, fontSize = size }

withSave :: Cairo.Render () -> Cairo.Render ()
withSave action = Cairo.save >> action >> Cairo.restore

drawForm :: Form -> Cairo.Render ()
drawForm = renderGraphic transform (flip (>>)) (return ()) . unborder
  where
    transform (V3 (V3 m00 m01 tx) (V3 m10 m11 ty) _) graphic =
      withSave $ do
        Cairo.transform $ Cairo.Matrix m00 m10 m01 m11 tx ty
        graphic

filled :: RGB -> Bordered Shape -> Form
filled (r, g, b) = Bordered.mapInner fillShape
  where fillShape shape = primitive $ withSave $ do
          Cairo.setSourceRGB r g b
          renderShape shape
          Cairo.fill

outlined :: LineStyle -> Bordered Shape -> Form
outlined style = Bordered.mapInner outlineShape
  where outlineShape shape = primitive $ withSave $ do
          applyLineStyle style
          renderShape shape
          Cairo.stroke

applyLineStyle :: LineStyle -> Cairo.Render ()
applyLineStyle style = do
  Cairo.setSourceRGB r g b
  Cairo.setLineWidth $ lineWidth style
  Cairo.setLineCap   $ convertLineCap  $ cap style
  Cairo.setLineJoin  $ convertLineJoin $ lineJoin style
  Cairo.setDash (dash style) (dashOffset style)
  where
    (r, g, b) = color style

convertLineCap :: LineCap -> Cairo.LineCap
convertLineCap Flat   = Cairo.LineCapButt
convertLineCap Round  = Cairo.LineCapRound
convertLineCap Padded = Cairo.LineCapSquare

convertLineJoin :: LineJoin -> Cairo.LineJoin
convertLineJoin Clipped = Cairo.LineJoinBevel
convertLineJoin Smooth  = Cairo.LineJoinRound
convertLineJoin Sharp   = Cairo.LineJoinMiter

gap :: Double -> Double -> Form
gap w h = bordered (0.5,0.5) w h empty

text :: TextStyle -> String -> Form
text style content = Bordered border $ Graphic.primitive $ withSave $ do
                       Cairo.setSourceRGB r g b
                       showLayout pLayout
  where
    border = Border.fromBoundingBox (V2 0 0, V2 width height)
    (r, g, b) = textColor style

    pLayout :: PangoLayout
    pLayout = unsafePerformIO $ do
      pContext <- getContext =<< getFontDescription
      layoutText pContext content
    (PangoRectangle _ _ width height)
      = snd $ unsafePerformIO $ layoutGetExtents pLayout

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
