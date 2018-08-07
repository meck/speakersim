{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Speakersim
import           Graphics.Gloss.Raster.Field
import           Graphics.Gloss.Interface.IO.Interact
import           Control.Monad.Reader
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
                        optRes :: Int -- | Resolution
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

resol :: Int
resol = 1

zoomFact :: Float
zoomFact = 5

iDisplay :: Display
iDisplay = InWindow winName initalWinSize initalWinPos

data World = World { evnt :: Env
                   , spkrs :: [Speaker]
                   , viewSize :: (Int, Int)
                   , viewOrig :: (Float, Float)
                   , pixPerM :: Float
                   } deriving (Generic, Show)

instance ToJSON World
instance FromJSON World






makePict :: World -> IO Picture
makePict w =
  return
    $ pictures
    $ uncurry makePicture (viewSize w) resol resol (pointColor w)
    : (drawSpeaker (pixPerM w) <$> spkrs w)

pointColor :: World -> Point -> Color
pointColor (World e sp vs _ ppm) p = dbToCol totDb
 where
  p'    = uncurry bimap (bimap' ((*) . (/ 2) . (/ ppm) . fromIntegral) vs) p -- TODO /2 for 2x res?
  totDb = audioVecToSpl $ runReader (totalAtPoint p' sp) e

dbToCol :: Double -> Color
dbToCol l = rgb' scalR scalG 0
 where
  sMax  = 100
  sMin  = 60
  sMid  = sMin + (sMax - sMin) / 2
  scalR = realToFrac $ scal $ (l - sMin) / (sMax - sMin)
  scalG = realToFrac $ scal $ (l - sMin) / (sMid - sMin)
  scal x | x < 0     = 0
         | x > 1     = 1
         | otherwise = x


eventHandler :: Event -> World -> IO World
eventHandler e w = case e of
  EventKey (Char '-') Down _ _ -> return $ w { pixPerM = pixPerM w - zoomFact }
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
  { evnt     = Env (Just Atmos {tmp = 20, hum = 0.5, pres = 101.325}) 20000.0
  , spkrs    = defSpeak
  , viewSize = initalWinSize
  , viewOrig = (0, 0)
  , pixPerM  = 20
  }

defSpeak :: [Speaker]
defSpeak = [idealSpeaker]
  -- [ idealSpeaker { pos = (0.0, 0.0), dly = 0.0025 }
  -- , idealSpeaker { pos = (0.0, -0.8575), polInv = True }
  -- ]

main :: IO ()
main = do
  Options {..} <- execParser parseOptions

  world        <- case optFile of
    Nothing -> return defWorld
    Just fp -> do
      f <- B.readFile fp
      case decode f of
        Nothing -> error $ "Error reading file: " ++ show fp
        Just w' -> return w'

  interactIO (InWindow "sub" initalWinSize initalWinPos)
             black
             world
             makePict
             eventHandler
             (const $ return ())

