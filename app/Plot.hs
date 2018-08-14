{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Plot (PlotWorld(..), showPlot) where

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
                       , scaleMin :: Float
                       , scaleMax :: Float
                       } deriving (Generic, Show)

instance ToJSON PlotWorld
instance FromJSON PlotWorld

zoomFact :: Float
zoomFact = 5

moveFact :: Float
moveFact = 1000

gradientColors :: [Color]
gradientColors = [black, blue, azure, green, yellow, red]

dbToCol :: PlotWorld -> Double -> Color
dbToCol PlotWorld { scaleMin, scaleMax } v = valToCol gradientDelta vClamped
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

pointColor :: PlotWorld -> Point -> Color
pointColor w@PlotWorld { frqAtmos, spkrs, viewSize, viewOrig, pixPerM } p =
  dbToCol w totDb
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

makePict :: PlotWorld -> Int -> IO Picture
makePict w@PlotWorld { viewSize, showGrid, pixPerM, viewOrig, spkrs } r = pure
  $ pictures [sim, speakers, grid]
 where
  sim  = uncurry makePicture viewSize r r (pointColor w)
  grid = if showGrid then drawGrid viewSize pixPerM else Blank
  speakers =
    uncurry translate viewOrig $ pictures $ drawSpeaker pixPerM <$> spkrs

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

showPlot :: String -> (Int, Int) -> Int -> PlotWorld -> IO ()
showPlot name initalPos res w@PlotWorld { viewSize } = interactIO
  (InWindow name viewSize initalPos)
  black
  w
  (`makePict` res)
  eventHandler
  (const $ return ())
