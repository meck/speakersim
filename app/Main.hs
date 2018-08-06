module Main where

import           Speakersim
import           Graphics.Gloss.Raster.Field
import           Graphics.Gloss.Interface.IO.Interact
import           Graphics.Gloss
import           Control.Monad.Reader
import           Data.Complex
import           Data.IORef
import           Data.Bifunctor

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
                   }



iWorld = World
  { evnt     = Env (Just Atmos {tmp = 20, hum = 0.5, pres = 101.325}) 100.0
  , spkrs    = iSpeak
  , viewSize = initalWinSize
  , viewOrig = (0, 0)
  , pixPerM  = 20
  }

iSpeak =
  [ idealSpeaker { pos = (0.0, 0.0), dly = 0.0025 }
  , idealSpeaker { pos = (0.0, -0.8575), polInv = True }
  ]

makePict :: World -> IO Picture
makePict w =
  return
    $ pictures
    $ uncurry makePicture (viewSize w) resol resol (pointColor w)
    : (drawSpeaker (pixPerM w) <$> spkrs w)

pointColor :: World -> Point -> Color
pointColor (World e sp vs vo ppm) p = dbToCol totDb
 where
  p'    = uncurry bimap (bimap' ((*) . (/ 2) . (/ ppm) . fromIntegral) vs) p -- TODO /2 for 2x res?
  totDb = audioVecToSpl $ runReader (totalAtPoint p' sp) e

dbToCol :: Double -> Color
dbToCol x = rgb' scalR scalG 0
 where
  sMax  = 100
  sMin  = 60
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
    -- EventKey k Down _ _ -> do
    --   print k
    --   return w
    EventKey (Char '-') Down _ _ ->
      return $ w { pixPerM = pixPerM w - zoomFact }
    EventKey (Char '=') Down _ _ ->
      return $ w { pixPerM = pixPerM w + zoomFact }
    EventKey{}    -> return w
    EventMotion{} -> return w
    EventResize s -> do
      -- controllerSetRedraw c'
      return w { viewSize = s }


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
  , res    = return (1 :+ 0)
  , size   = (1, 1)
  }

bimap' :: Bifunctor p => (a -> d) -> p a a -> p d d
bimap' f = bimap f f
