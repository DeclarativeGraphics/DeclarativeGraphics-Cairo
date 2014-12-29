module Graphics.Declarative.Backend.Cairo where


import System.IO.Unsafe (unsafePerformIO)

import Graphics.Rendering.Pango hiding (Color, Font)
import qualified Graphics.Rendering.Cairo as Cairo

import Graphics.Declarative.Graphic as Graphic
import Graphics.Declarative.Framed as Framed
import Graphics.Declarative.Frame as Frame
import Graphics.Declarative.Shape as Shape

type RGB = (Double, Double, Double)

type Form = Framed (Graphic (Cairo.Render()))

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
drawForm = renderGraphic move (flip (>>)) . unframe
  where move offset graphic = do
          Cairo.save
          uncurry Cairo.translate offset
          graphic
          Cairo.restore

filled :: RGB -> Framed Shape -> Form
filled (r, g, b) = Framed.map fillShape
  where fillShape shape = primitive $ do
          Cairo.save
          Cairo.setSourceRGB r g b
          renderShape shape
          Cairo.fill
          Cairo.restore

outlined :: LineStyle -> Framed Shape -> Form
outlined style = Framed.map outlineShape
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

text :: TextStyle -> String -> Form
text style content = Framed (Frame 0 0 width height) $ Graphic.primitive $ do
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

debugFrame :: Form -> Form
debugFrame graphic =
  collapseFrame (outlined (solid (1, 0, 0)) $ circle 2)
  `Framed.atop`
  outlined (solid (1, 0, 0)) (fromFrame $ getFrame graphic)
  `Framed.atop`
  graphic

debugWithSize :: Form -> Form
debugWithSize graphic =
  collapseFrame (Framed.move (left, bottom) $ text monoStyle $ "width : " ++ show w ++ "\nheight: " ++ show h)
  `Framed.atop`
  debugFrame graphic
  where
    (Frame left _ _ bottom) = getFrame graphic
    w = graphicWidth graphic
    h = graphicHeight graphic
    monoStyle = defaultTextStyle { fontFamily = "Monospace", fontSize = 8 }
