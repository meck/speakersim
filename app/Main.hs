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
import           Data.Complex
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

iDisplay :: Display
iDisplay = InWindow winName initalWinSize initalWinPos

data World = World { evnt :: Env
                   , spkrs :: [Speaker]
                   , viewSize :: (Int, Int)
                   , viewOrig :: (Float, Float)
                   , pixPerM :: Float             -- TODO Non Negative type
                   } deriving (Generic, Show)

instance ToJSON World
instance FromJSON World

makePict :: Int -> World -> IO Picture
makePict r w =
  return
    $ pictures
    $ uncurry makePicture (viewSize w) r r (pointColor w)
    : (drawSpeaker (pixPerM w) <$> spkrs w)

pointColor :: World -> Point -> Color
pointColor (World e sp vs _ ppm) p = dbToCol totDb
 where
  p'    = uncurry bimap (bimap' ((*) . (/ 2) . (/ ppm) . fromIntegral) vs) p -- TODO /2 for 2x res?
  totDb = audioVecToSpl $ runReader (totalAtPoint p' sp) e

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



eventHandler :: Event -> World -> IO World
eventHandler e w = case e of
  EventKey (Char '-') Down _ _ -> return $ w
    { pixPerM = if pixPerM w > zoomFact then pixPerM w - zoomFact else pixPerM w -- TODO Change type to cleanup
    }
  EventKey (Char '=') Down _ _ -> return $ w { pixPerM = pixPerM w + zoomFact }
  EventKey{}                   -> return w
  EventMotion{}                -> return w
  EventResize s                -> return w { viewSize = s }


drawSpeaker :: Float -> Speaker -> Picture
drawSpeaker sc s =
  color (greyN 0.7)
    $ uncurry translate (bimap' (* sc) (pos s))
    $ scale sc sc
    $ polygon
    $ uncurry rectanglePath
    $ size s

idealSpeaker :: Speaker
idealSpeaker = Speaker
  { pos    = (0, 0)
  , level  = 0
  , dly    = 0
  , polInv = False
  , res    = const (1 :+ 0)
  , size   = (1, 1)
  }

bimap' :: Bifunctor p => (a -> d) -> p a a -> p d d
bimap' f = bimap f f

defWorld :: World
defWorld = World
  { evnt     = Env (Just Atmos {tmp = 20, hum = 0.5, pres = 101.325}) 100.0
  , spkrs    = defSpeak
  , viewSize = initalWinSize
  , viewOrig = (0, 0)
  , pixPerM  = 20
  }

defSpeak :: [Speaker]
-- defSpeak = [idealSpeaker]
defSpeak =
  [ idealSpeaker { pos = (0.0, 10), dly = 0.0025 }
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
