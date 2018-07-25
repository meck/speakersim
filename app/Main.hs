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
  { evnt     = Env 20 0.5 300
  , spkrs    = initalSpkrs
  , viewSize = initalWinSize
  , viewOrig = (0, 0)
  }

makePict :: World -> IO Picture
makePict w = return $ uncurry makePicture (viewSize w) 1 1 (pointColor w) -- TODO fetch size and zoom

pointColor :: World -> Point -> Color -- TODO Clean up add offset
pointColor (World e sp vs vo) p = dbToCol totDb
 where
  p'    = (fst p * 10, snd p * 10)
  totDb = audioVecToSpl $ runReader (totalAtPoint p' sp) e

dbToCol :: Double -> Color
-- dbToCol x = rgb (3.0 * x') (2.0 * (1 - x')) 0 where 
dbToCol x = rgb x'' (1 - x'') 0
 where
  rMaxGmin = 120
  rMinGMax = 50
  d        = rMaxGmin - rMinGMax
  x'       = realToFrac $ (x - rMinGMax) / d
  x''      = x' - 1 / realToFrac d

initalSpkrs =
  [ idealSpeaker { pos = (1, 1) }
  , idealSpeaker { pos = (-1, -1) }
  , idealSpeaker { pos = (-1, 1) }
  , idealSpeaker { pos = (1, -1) }
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
