module Main where

import           Lib
import           Graphics.Gloss.Raster.Field
import           Graphics.Gloss.Interface.IO.Interact
import           Graphics.Gloss
import           Control.Monad.Reader
import           Data.Complex
import           Data.IORef
import           Graphics.Gloss.Data.ViewPort
import qualified Graphics.Gloss.Data.Point.Arithmetic
                                               as PA

winName :: String
winName = "Sub Plot"

initalWinSize :: (Int, Int)
initalWinSize = (1000, 1000)

initalWinPos :: (Int, Int)
initalWinPos = (100, 100)

resol :: Int
resol = 1

pixPerMeter = 20

iDisplay :: Display
iDisplay = InWindow winName initalWinSize initalWinPos

data World = World { evnt :: Env
                   , spkrs :: [Speaker]
                   , viewSize :: (Int, Int)
                   , viewOrig :: (Float, Float) }


iWorld = World
  { evnt     = Env (Just Atmos {tmp = 20, hum = 0.5, pres = 101.325}) 1000.0
  , spkrs    = iSpeak
  , viewSize = initalWinSize
  , viewOrig = (0, 0)
  }

iSpeak =
  [idealSpeaker { pos = (0.0, 5.0) }, idealSpeaker { pos = (0.0, -5.0) }]

makePict :: World -> IO Picture
makePict w =
  return
    $ pictures
    $ uncurry makePicture (1000, 1000) resol resol (pointColor w)
    : (drawSpeaker <$> spkrs w)

pointColor :: World -> Point -> Color
pointColor (World e sp vs vo) p = dbToCol totDb
 where
  p'    = bimap (pixPerMeter *) p
-- p'    = bimap (/ pixPerMeter) p
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
main = do
  cont <- newIORef (undefined :: Controller)
  interactIO (InWindow "sub" initalWinSize initalWinPos)
             black
             iWorld
             makePict
             (eventHandler cont)
             (writeIORef cont)

eventHandler :: IORef Controller -> Event -> World -> IO World
eventHandler c e w = do
  c' <- readIORef c
  case e of
    -- EventKey (MouseButton LeftButton) Down _ _ -> do
    --   _ <- controllerModifyViewPort
    --     c'
    --     (\vp -> return $ vp { viewPortScale = 4.0 })
      -- return w
    EventKey{}    -> return w
    EventMotion{} -> return w
    EventResize s -> do
      -- controllerSetRedraw c'
      return w { viewSize = s }


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
