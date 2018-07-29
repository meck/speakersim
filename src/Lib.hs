module Lib
  ( Cord
  , Time
  , Freq
  , Temp
  , Hum
  , AudioVect
  , Env(..)
  , Speaker(..)
  , Resp
  , totalAtPoint
  , audioVecToSpl
  )
where

import           Control.Applicative            ( liftA2 )
import           Data.Complex                   ( Complex(..)
                                                , magnitude
                                                , cis
                                                , mkPolar
                                                )
import           Control.Monad.Reader


-- | An (x,y) cordinate in meters
type Cord = (Float, Float)

-- | Time in sec
type Time = Double

-- | Freq in Hz
type Freq = Double

-- | Temp in C
type Temp = Double

-- | Humidity in %
type Hum = Double

-- | Vector with magnitude and phase
type AudioVect = Complex Double

-- | The Enviorment Of a speaker and frequency of intrest
data Env = Env { tmp :: Temp
               , hum :: Hum
               , frq :: Freq }

data Speaker = Speaker { pos :: Cord              -- The physical placment of a speaker in meters
                       , level :: Float           -- The level of the speaker in dB (<= 0)
                       , polInv :: Bool           -- Polarity inverted
                       , dly :: Time              -- the processing delay in seconds
                       , res :: Freq -> AudioVect -- takes a Frequncy and returns Pa measured @ 1 meter
                       , size :: (Float,Float)    -- the Physical size of the speaker
                       }

-- | A Audio respone awaiting an enviorment
type Resp = Reader Env AudioVect

-- | Total Response at point in the plane
totalAtPoint :: Cord -> [Speaker] -> Resp
totalAtPoint c ss = foldr (liftA2 (+)) (pure (0 :+ 0)) $ respAtPoint c <$> ss

-- | Respone of one speaker at point in the plane
respAtPoint :: Cord -> Speaker -> Resp
respAtPoint p s = do
  e <- ask
  let d = dist p $ pos s
      t = propTime e (realToFrac d) + dly s
      f = frq e
      i = polInv s
  return $ attAtmos d e $ attDist d $ phaseInv i $ dlyPhase t f $ res s f

-- | utillity for converting a vector of Pa to SPL
audioVecToSpl :: AudioVect -> Double
audioVecToSpl = spToSpl . magnitude

attDist :: Float -> AudioVect -> AudioVect
attDist 0 x = x
attDist d x = mkPolar (1 / realToFrac d) 0 * x

attAtmos :: Float -> Env -> AudioVect -> AudioVect
attAtmos d e x = x -- TODO Implement   

dlyPhase :: Time -> Freq -> AudioVect -> AudioVect
dlyPhase t f x = cis (f * t * pi * 2) * x

phaseInv :: Bool -> AudioVect -> AudioVect
phaseInv p x = if p then cis pi * x else x

speedOfSound :: Env -> Double
speedOfSound _ = 343.0 -- TODO Make Atomosferic dependent

dist :: Cord -> Cord -> Float
dist a b = sqrt $ ((fst b - fst a) ^ 2) + ((snd b - snd a) ^ 2)

propTime :: Env -> Double -> Time
propTime e d = d / speedOfSound e

splToSp :: Double -> Double
splToSp x = 2.0e-5 * (10 ** (x / 20))

spToSpl :: Double -> Double
spToSpl x = 20 * logBase 10 (x / 2.0e-5)


