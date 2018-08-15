{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Plot (PlotState(..), showPlot) where

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

data PlotState = PlotState { frqAtmos :: FrqAtmos
                       , spkrs :: [Speaker]
                       , viewSize :: (Int, Int)
                       , viewOrig :: (Float, Float)
                       , pixPerM :: Float
                       , showGrid :: Bool
                       , scaleMin :: Float
                       , scaleMax :: Float
                       } deriving (Generic, Show)

instance ToJSON PlotState
instance FromJSON PlotState

zoomFact :: Float
zoomFact = 5

moveFact :: Float
moveFact = 1000

gradientColors :: [Color]
gradientColors = [black, blue, azure, green, yellow, red]

dbToCol :: PlotState -> Double -> Color
dbToCol PlotState { scaleMin, scaleMax } v = valToCol gradientDelta vClamped
 where
  delta         = 1 / realToFrac (pred $ length gradientColors)
  gradientDelta = zip [0, delta ..] gradientColors
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

pointColor :: PlotState -> Point -> Color
pointColor s@PlotState { frqAtmos, spkrs, viewSize, viewOrig, pixPerM } p =
  dbToCol s totDb
 where
  liftPoint f a = uncurry bimap (bimap' f a)
  pScale = liftPoint ((*) . (/ 2) . (/ pixPerM) . fromIntegral) viewSize
  pTrans = liftPoint ((+) . (/ pixPerM) . negate) viewOrig
  totDb =
    audioVecToSpl $ runReader (totalAtPoint (pTrans $ pScale p) spkrs) frqAtmos

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

makePict :: PlotState -> Int -> IO Picture
makePict s@PlotState { viewSize, showGrid, pixPerM, viewOrig, spkrs } r = pure
  $ pictures [sim, speakers, grid]
 where
  sim  = uncurry makePicture viewSize r r (pointColor s)
  grid = if showGrid then drawGrid viewSize pixPerM else Blank
  speakers =
    uncurry translate viewOrig $ pictures $ drawSpeaker pixPerM <$> spkrs

eventHandler :: Event -> PlotState -> IO PlotState
eventHandler e s@PlotState { pixPerM, showGrid, viewOrig } = case e of
  EventKey (Char '-') Down _ _ ->
    return $ s { pixPerM = pixPerM `subToZero` zoomFact }
  EventKey (Char '=') Down _ _ -> return $ s { pixPerM = pixPerM + zoomFact }
  EventKey (Char 'g') Down _ _ -> return $ s { showGrid = not showGrid }
  EventKey (Char 'h') Down _ _ ->
    return $ s { viewOrig = mapX (subtract movePix) viewOrig }
  EventKey (Char 'j') Down _ _ ->
    return $ s { viewOrig = mapY (subtract movePix) viewOrig }
  EventKey (Char 'k') Down _ _ ->
    return $ s { viewOrig = mapY (+ movePix) viewOrig }
  EventKey (Char 'l') Down _ _ ->
    return $ s { viewOrig = mapX (+ movePix) viewOrig }
  EventKey{}       -> return s
  EventMotion{}    -> return s
  EventResize size -> return s { viewSize = size }
 where
  subToZero a b = if a - b <= 0 then a else a - b
  movePix = moveFact / pixPerM
  mapX f (x, y) = (f x, y)
  mapY = fmap

bimap' :: Bifunctor p => (a -> d) -> p a a -> p d d
bimap' f = bimap f f

showPlot :: String -> (Int, Int) -> Int -> PlotState -> IO ()
showPlot name initalPos res s@PlotState { viewSize } = interactIO
  (InWindow name viewSize initalPos)
  black
  s
  (`makePict` res)
  eventHandler
  (const $ return ())
