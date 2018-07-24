module Main where

import           Lib
import           Graphics.Gloss.Raster.Field
import           Graphics.Gloss
import           Control.Monad.Reader
import           Debug.Trace
import           Data.Complex


main :: IO ()
main = display (InWindow "sub" (1000, 1000) (100, 100)) black
  $ makePicture 1000 1000 1 1 drawField

drawField :: Point -> Color
drawField p = col
 where
  p'    = (fst p * 10, snd p * 10)
  totDb = audioVecToSpl $ runReader (totalAtPoint p' spkArray) (Env 20 0.5 500)
  col   = dbToCol totDb

spkArray =
  [ idealSpeaker { pos = (1, 0) }
  , idealSpeaker { pos = (-1, 0) }
  , idealSpeaker { pos = (0, 0) }
  ]

test p = audioVecToSpl $ runReader (totalAtPoint p spkArray) (Env 20 0.5 100)

dbToCol :: Double -> Color
dbToCol x = rgb (3.0 * x') (2.0 * (1 - x')) 0 where x' = realToFrac $ x / 150

idealSpeaker :: Speaker
idealSpeaker = Speaker
  { pos   = (0, 0)
  , level = 0
  , dly   = 0
  , res   = return (1 :+ 0)
  , size  = (1, 1)
  }
