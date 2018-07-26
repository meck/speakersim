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

initalDisplay :: Display
initalDisplay = InWindow winName initalWinSize initalWinPos

data World = World { evnt :: Env
                   , spkrs :: [Speaker]
                   , viewSize :: (Int, Int)
                   , viewOrig :: (Float, Float) }


initalWorld = World
  { evnt     = Env 20 0.5 50
  , spkrs    = initalSpkrs
  , viewSize = initalWinSize
  , viewOrig = (0, 0)
  }

makePict :: World -> IO Picture
makePict w = return $ uncurry makePicture (viewSize w) 1 1 (pointColor w) -- TODO fetch size and zoom

pointColor :: World -> Point -> Color -- TODO Clean up add offset
pointColor (World e sp vs vo) p = dbToCol totDb
 where
  p'    = (fst p * 20, snd p * 20)
  totDb = audioVecToSpl $ runReader (totalAtPoint p' sp) e

dbToCol :: Double -> Color
dbToCol x = rgb' scalR scalG 0
 where
  sMax  = 130
  sMin  = 70
  sMid  = sMin + (sMax - sMin) / 2
  scalR = realToFrac $ scal $ (x - sMin) / (sMax - sMin)
  scalG = realToFrac $ scal $ (x - sMin) / (sMid - sMin)
  scal x | x < 0     = 0
         | x > 1     = 1
         | otherwise = x

initalSpkrs =
  [ idealSpeaker { pos = (0.75, -1), dly = 0.040 }
  , idealSpeaker { pos = (0.25, -1) }
  , idealSpeaker { pos = (0, -1) }
  , idealSpeaker { pos = (-0.25, -1) }
  , idealSpeaker { pos = (-0.75, -1), dly = 0.040 }
  ]

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
  EventResize s -> return $ w { viewSize = s }

idealSpeaker :: Speaker
idealSpeaker = Speaker
  { pos   = (0, 0)
  , level = 0
  , dly   = 0
  , res   = return (1 :+ 0)
  , size  = (1, 1)
  }
