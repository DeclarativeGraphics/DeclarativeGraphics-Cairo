module Graphics.Declarative.Cairo.Form where


import System.IO.Unsafe (unsafePerformIO)

import           Graphics.Rendering.Pango hiding (Color, Font)
import qualified Graphics.Rendering.Cairo as Cairo

import           Graphics.Declarative.Graphic  as Graphic
import           Graphics.Declarative.Bordered as Bordered
import qualified Graphics.Declarative.Border   as Border

import           Graphics.Declarative.Cairo.Shape as Shape

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

drawForm :: Form -> Cairo.Render ()
drawForm = renderGraphic move (flip (>>)) . unborder
  where move offset graphic = do
          Cairo.save
          uncurry Cairo.translate offset
          graphic
          Cairo.restore

filled :: RGB -> Bordered Shape -> Form
filled (r, g, b) = Bordered.map fillShape
  where fillShape shape = primitive $ do
          Cairo.save
          Cairo.setSourceRGB r g b
          renderShape shape
          Cairo.fill
          Cairo.restore

outlined :: LineStyle -> Bordered Shape -> Form
outlined style = Bordered.map outlineShape
  where outlineShape shape = primitive $ do
          Cairo.save
          applyLineStyle style
          renderShape shape
          Cairo.stroke
          Cairo.restore

solid :: RGB -> LineStyle
solid col = defaultLineStyle { color = col }

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

empty :: Form
empty = gap 0 0

gap :: Double -> Double -> Form
gap w h = bordered (0.5,0.5) w h (Graphic.primitive $ return ())

text :: TextStyle -> String -> Form
text style content = Bordered border $ Graphic.primitive $ do
                       Cairo.save
                       Cairo.setSourceRGB r g b
                       showLayout pLayout
                       Cairo.restore
  where
    border = Border.fromBoundingBox ((0, 0), (width, height))
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
