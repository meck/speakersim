{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

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
                                                , decode
                                                )
import           GHC.Generics
import           Options.Applicative
import qualified Data.ByteString.Lazy          as B

data Options = Options {
                        optRes :: Int             -- | Resolution
                       ,optFile :: Maybe FilePath -- | Json file with the world
                       } deriving Show

options :: Parser Options
options =
  Options
    <$> option
          auto
          (short 'r' <> long "resolution" <> metavar "INT" <> value 1 <> help
            "The resolution of the simulation"
          )
    <*> optional
          (strArgument
            (  metavar "FILENAME"
            <> help "A Json file containing the state of the world"
            )
          )

parseOptions :: ParserInfo Options
parseOptions = info
  (options <**> helper)
  (fullDesc <> progDesc "Visualize speaker interaction" <> header "SpeakerPlot")

winName :: String
winName = "Speak Plot"

initalWinSize :: (Int, Int)
initalWinSize = (1000, 1000)

initalWinPos :: (Int, Int)
initalWinPos = (100, 100)

zoomFact :: Float
zoomFact = 5

moveFact :: Float
moveFact = 1000

iDisplay :: Display
iDisplay = InWindow winName initalWinSize initalWinPos

data World = World { frqAtmos :: FrqAtmos
                   , spkrs :: [Speaker]
                   , viewSize :: (Int, Int)
                   , viewOrig :: (Float, Float)
                   , pixPerM :: Float
                   , showGrid :: Bool
                   } deriving (Generic, Show)

instance ToJSON World
instance FromJSON World

makePict :: Int -> World -> IO Picture
makePict r w = return $ pictures [sim, speakers, grid]
 where
  sim  = uncurry makePicture (viewSize w) r r (pointColor w)
  grid = if showGrid w then drawGrid (viewSize w) (pixPerM w) else Blank
  speakers =
    uncurry translate (viewOrig w)
      $   pictures
      $   drawSpeaker (pixPerM w)
      <$> spkrs w

pointColor :: World -> Point -> Color
pointColor (World e sp vs vo ppm _) p = dbToCol totDb
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
  gradientColors = [black, blue, green, red]
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

drawGrid :: (Int, Int) -> Float -> Picture -- TODO Offset grid when moving
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

eventHandler :: Event -> World -> IO World -- TODO Cleanup, lenses?
eventHandler e w = case e of
  EventKey (Char '-') Down _ _ ->
    return $ w { pixPerM = pixPerM w `subToZero` zoomFact }
  EventKey (Char '=') Down _ _ -> return $ w { pixPerM = pixPerM w + zoomFact }
  EventKey (Char 'g') Down _ _ -> return $ w { showGrid = not $ showGrid w }
  EventKey (Char 'h') Down _ _ ->
    let newOX = fst (viewOrig w) - moveFact / pixPerM w
        newOY = snd (viewOrig w)
    in  return $ w { viewOrig = (newOX, newOY) }
  EventKey (Char 'j') Down _ _ ->
    let newOY = snd (viewOrig w) - moveFact / pixPerM w
        newOX = fst (viewOrig w)
    in  return $ w { viewOrig = (newOX, newOY) }
  EventKey (Char 'k') Down _ _ ->
    let newOY = snd (viewOrig w) + moveFact / pixPerM w
        newOX = fst (viewOrig w)
    in  return $ w { viewOrig = (newOX, newOY) }
  EventKey (Char 'l') Down _ _ ->
    let newOX = fst (viewOrig w) + moveFact / pixPerM w
        newOY = snd (viewOrig w)
    in  return $ w { viewOrig = (newOX, newOY) }

  EventKey{}    -> return w
  EventMotion{} -> return w
  EventResize s -> return w { viewSize = s }
  where subToZero a b = if a - b <= 0 then a else a - b

bimap' :: Bifunctor p => (a -> d) -> p a a -> p d d
bimap' f = bimap f f

defWorld :: World
defWorld = World
  { frqAtmos = FrqAtmos
    { atmos = Just Atmos {tmp = 20, hum = 0.5, pres = 101.325}
    , freq  = 100.0
    }
  , spkrs    = defSpeak
  , viewSize = initalWinSize
  , viewOrig = (0, 0)
  , pixPerM  = 20
  , showGrid = True
  }

defSpeak :: [Speaker]
-- defSpeak = [idealSpeaker]
defSpeak =
  [ idealSpeaker { pos = (0.0, 0), dly = 0.0025 }
  , idealSpeaker { pos = (0.0, -0.8575), polInv = True }
  ]

main :: IO ()
main = do
  Options { optFile, optRes } <- execParser parseOptions

  world                       <- case optFile of
    Nothing -> return defWorld
    Just fp -> do
      f <- B.readFile fp
      case decode f of
        Nothing -> error $ "Error reading file: " ++ show fp
        Just w' -> return w'


  interactIO (InWindow "sub" initalWinSize initalWinPos)
             black
             world
             (makePict optRes)
             eventHandler
             (const $ return ())
