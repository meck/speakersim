{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Plot (PlotWorld(..), defaultPlot, showPlot) where

import           Speakersim
import           Graphics.Gloss.Raster.Field
import           Graphics.Gloss.Interface.IO.Interact
import           Control.Monad.Reader
import           Data.Colour                    ( blend )
import           Data.Colour.SRGB               ( RGB(..)
                                                , toSRGB
                                                , sRGB
                                                )
import           Data.Bifunctor
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           GHC.Generics

data PlotWorld = PlotWorld { frqAtmos :: FrqAtmos
                       , spkrs :: [Speaker]
                       , viewSize :: (Int, Int)
                       , viewOrig :: (Float, Float)
                       , pixPerM :: Float
                       , showGrid :: Bool
                       } deriving (Generic, Show)

instance ToJSON PlotWorld
instance FromJSON PlotWorld

defaultPlot :: PlotWorld
defaultPlot = PlotWorld
  { frqAtmos = FrqAtmos
    { atmos = Just Atmos {tmp = 20, hum = 0.5, pres = 101.325}
    , freq  = 100.0
    }
  , spkrs    = [idealSpeaker]
  , viewSize = (1000, 1000)
  , viewOrig = (0, 0)
  , pixPerM  = 20
  , showGrid = True
  }

-- defSpeak =
--   [ idealSpeaker { pos = (0.0, 0), dly = 0.0025 }
--   , idealSpeaker { pos = (0.0, -0.8575), polInv = True }
--   ]

winName :: String -- TODO Move to PlotWorld
winName = "Speak Plot"

initalWinPos :: (Int, Int)
initalWinPos = (100, 100)

zoomFact :: Float
zoomFact = 5

moveFact :: Float
moveFact = 1000

makePict :: Int -> PlotWorld -> IO Picture
makePict r w = return $ pictures [sim, speakers, grid]
 where
  sim  = uncurry makePicture (viewSize w) r r (pointColor w)
  grid = if showGrid w then drawGrid (viewSize w) (pixPerM w) else Blank
  speakers =
    uncurry translate (viewOrig w)
      $   pictures
      $   drawSpeaker (pixPerM w)
      <$> spkrs w

pointColor :: PlotWorld -> Point -> Color
pointColor (PlotWorld e sp vs vo ppm _) p = dbToCol totDb
 where
  liftPoint f a = uncurry bimap (bimap' f a)
  pScale = liftPoint ((*) . (/ 2) . (/ ppm) . fromIntegral) vs
  pTrans = liftPoint ((+) . (/ ppm) . negate) vo
  totDb  = audioVecToSpl $ runReader (totalAtPoint (pTrans $ pScale p) sp) e

dbToCol :: Double -> Color
dbToCol v = valToCol gradientDelta vClamped
 where
  scaleMax       = 90
  scaleMin       = 60
  gradientColors = [black, blue, azure, green, yellow, red]
  delta          = 1 / realToFrac (pred $ length gradientColors)
  gradientDelta  = zip [0, delta ..] gradientColors
  vClamped | v > realToFrac scaleMax = 1
           | v < realToFrac scaleMin = 0
           | otherwise = (realToFrac v - scaleMin) / (scaleMax - scaleMin)
  valToCol (c : cc : cs) val | val <= fst cc = mix (1 - x) (snd c) (snd cc)
                             | otherwise     = valToCol (cc : cs) val
    where x = (val - fst c) / (fst cc - fst c)
  valToCol _ _ = error "Not enough colors in list"
  mix f c c' = makeColor (channelRed mixed)
                         (channelGreen mixed)
                         (channelBlue mixed)
                         1
   where
    (aR, aG, aB, _) = rgbaOfColor c
    (bR, bG, bB, _) = rgbaOfColor c'
    mixed           = toSRGB $ blend f (sRGB aR aG aB) (sRGB bR bG bB)

drawGrid :: (Int, Int) -> Float -> Picture
drawGrid (hSize, vSize) spacing =
  color (greyN 0.4) $ pictures $ pictures <$> [hLines, vLines]
 where
  hMax = fromIntegral hSize / 2
  hMin = negate hMax
  vMax = fromIntegral vSize / 2
  vMin = negate vMax
  hLine vPos = Line [(hMin, vPos), (hMax, vPos)]
  vLine hPos = Line [(hPos, vMin), (hPos, vMax)]
  hLines = hLine <$> [0, spacing .. vMax] ++ [0, negate spacing .. vMin]
  vLines = vLine <$> [0, spacing .. hMax] ++ [0, negate spacing .. hMin]

drawSpeaker :: Float -> Speaker -> Picture
drawSpeaker sc s =
  uncurry translate (bimap' (* sc) (pos s)) $ scale sc sc $ pictures
    [spk, frame]
 where
  spk   = color (greyN 0.7) $ uncurry rectangleSolid $ size s
  frame = color black $ uncurry rectangleWire $ size s

eventHandler :: Event -> PlotWorld -> IO PlotWorld
eventHandler e w@PlotWorld { pixPerM, showGrid, viewOrig } = case e of
  EventKey (Char '-') Down _ _ ->
    return $ w { pixPerM = pixPerM `subToZero` zoomFact }
  EventKey (Char '=') Down _ _ -> return $ w { pixPerM = pixPerM + zoomFact }
  EventKey (Char 'g') Down _ _ -> return $ w { showGrid = not showGrid }
  EventKey (Char 'h') Down _ _ ->
    return $ w { viewOrig = mapX (subtract movePix) viewOrig }
  EventKey (Char 'j') Down _ _ ->
    return $ w { viewOrig = mapY (subtract movePix) viewOrig }
  EventKey (Char 'k') Down _ _ ->
    return $ w { viewOrig = mapY (+ movePix) viewOrig }
  EventKey (Char 'l') Down _ _ ->
    return $ w { viewOrig = mapX (+ movePix) viewOrig }
  EventKey{}    -> return w
  EventMotion{} -> return w
  EventResize s -> return w { viewSize = s }
 where
  subToZero a b = if a - b <= 0 then a else a - b
  movePix = moveFact / pixPerM
  mapX f (x, y) = (f x, y)
  mapY = fmap

bimap' :: Bifunctor p => (a -> d) -> p a a -> p d d
bimap' f = bimap f f

showPlot :: Int -> PlotWorld -> IO ()
showPlot res w@PlotWorld { viewSize } = interactIO
  (InWindow winName viewSize initalWinPos)
  black
  w
  (makePict res)
  eventHandler
  (const $ return ())
