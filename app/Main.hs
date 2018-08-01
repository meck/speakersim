module Main where

import           Lib
import           Graphics.Gloss.Raster.Field
import           Graphics.Gloss.Interface.IO.Interact
import           Graphics.Gloss
import           Control.Monad.Reader
import           Data.Complex

winName :: String
winName = "Sub Plot"

initalWinSize :: (Int, Int)
initalWinSize = (1000, 1000)

initalWinPos :: (Int, Int)
initalWinPos = (100, 100)

resol :: Int
resol = 1

pixPerMeter = 10

initalDisplay :: Display
initalDisplay = InWindow winName initalWinSize initalWinPos

data World = World { evnt :: Env
                   , spkrs :: [Speaker]
                   , viewSize :: (Int, Int)
                   , viewOrig :: (Float, Float) }


initalWorld = World
  { evnt     = Env (Atmos {tmp = 20, hum = 0.5, pres = 101.325}) 10000.0
  , spkrs    = initalSpkrs
  , viewSize = initalWinSize
  , viewOrig = (0, 0)
  }

-- initalSpkrs = [idealSpeaker { pos = (0, 0) }, idealSpeaker { pos = (0.1, 0) }]

-- initalSpkrs =
--   [ idealSpeaker { pos = (0.0, 0.0), dly = 0.0025, polInv = True }
--   , idealSpeaker { pos = (0.0, 0.8575), polInv = False }
--   ]

initalSpkrs =
  [idealSpeaker { pos = (0.0, 5.0) }, idealSpeaker { pos = (0.0, -5.0) }]

makePict :: World -> IO Picture
makePict w =
  return
    $ pictures
    $ uncurry makePicture (viewSize w) resol resol (pointColor w)
    : (drawSpeaker <$> spkrs w)

pointColor :: World -> Point -> Color -- TODO Clean up add offset
pointColor (World e sp vs vo) p = dbToCol totDb
 where
  p'    = bimap (pixPerMeter *) p
  totDb = audioVecToSpl $ runReader (totalAtPoint p' sp) e

dbToCol :: Double -> Color
dbToCol x = rgb' scalR scalG 0
 where
  sMax  = 100
  sMin  = 65
  sMid  = sMin + (sMax - sMin) / 2
  scalR = realToFrac $ scal $ (x - sMin) / (sMax - sMin)
  scalG = realToFrac $ scal $ (x - sMin) / (sMid - sMin)
  scal x | x < 0     = 0
         | x > 1     = 1
         | otherwise = x


main :: IO ()
main = interactIO (InWindow "sub" initalWinSize initalWinPos)
                  black
                  initalWorld
                  makePict
                  eventHandler
                  (const $ return ())


eventHandler :: Event -> World -> IO World
eventHandler e w = case e of
  EventKey{}    -> return w
  EventMotion{} -> return w
  EventResize s -> return w { viewSize = s }

drawSpeaker :: Speaker -> Picture
drawSpeaker s =
  color blue
    $ uncurry translate (bimap (* pixPerMeter) (pos s))
    $ scale pixPerMeter pixPerMeter
    $ polygon
    $ uncurry rectanglePath
    $ size s

idealSpeaker :: Speaker
idealSpeaker = Speaker
  { pos    = (0, 0)
  , level  = 0
  , dly    = 0
  , polInv = False
  , res    = return (1 :+ 0)
  , size   = (1, 1)
  }

bimap :: (t -> b) -> (t, t) -> (b, b)
bimap f (x, y) = (f x, f y)
