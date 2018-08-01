module Lib
  ( Cord
  , Time
  , Freq
  , AudioVect
  , Atmos(..)
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

-- | Vector with magnitude and phase
type AudioVect = Complex Double

-- | The atmospheric properties
data Atmos = Atmos { tmp :: Double  -- Temperature in C
                   , hum :: Double  -- Humidity in %
                   , pres :: Double -- abient atmospheric pressue in kPa
                   }

data Env = Env Atmos Freq

data Speaker = Speaker { pos :: Cord              -- The physical placment of a speaker in meters
                       , level :: Double          -- The level of the speaker in dB (<= 0)
                       , polInv :: Bool           -- Polarity inverted
                       , dly :: Time              -- the processing delay in seconds
                       , res :: Freq -> AudioVect -- takes a Frequncy and returns Pa measured @ 1 meter
                       , size :: (Float,Float)    -- the Physical size of the speaker in meters
                       }

-- | A Audio respone awaiting an enviorment
type Resp = Reader Env AudioVect

-- | Total Response at point in the plane
totalAtPoint :: Cord -> [Speaker] -> Resp
totalAtPoint c ss = foldr (liftA2 (+)) (pure (0 :+ 0)) $ respAtPoint c <$> ss

-- | Respone of one speaker at point in the plane
respAtPoint :: Cord -> Speaker -> Resp
respAtPoint p s = do
  (Env a f) <- ask
  let d = realToFrac $ dist p $ pos s
      t = propTime a (realToFrac d) + dly s
      i = polInv s
  return $ attAtmos d f a $ attDist d $ phaseInv i $ dlyPhase t f $ res s f

-- | utillity for converting a vector of Pa to SPL
audioVecToSpl :: AudioVect -> Double
audioVecToSpl = spToSpl . magnitude

attDist :: Double -> AudioVect -> AudioVect
attDist 0 x = x
attDist d x = mkPolar (1 / d) 0 * x

attAtmos :: Double -> Freq -> Atmos -> AudioVect -> AudioVect
-- attAtmos d f a v = mkPolar alpha 0 * v
attAtmos d f a v = v
 where
  pt = pres a * exp ((-x) * alpha * d)
  x  = 0.1151
  alpha =
    8.686 * (f ** 2) * (1.84e-11 * ((pres a / pr) ** (-1)) * sqrt (t / t0) + y)
  y =
    (t / t0)
      ** (-5 / 2)
      *  (0.01275 * exp (-2239.1 / t) * (((fsq / frO) + frO) ** (-1)))
  z   = 0.1068 * exp (-3352 / t) * ((frN + (fsq / frN)) ** (-1))
  frO = relP * (24 + ((4.04e4 * h * (0.02 + h)) / (0.391 + h)))     -- oxygen relaxation frequency
  frN =
    relP
      * (1 / sqrt (t / t0))
      * (9 + 280 * h * exp (-4.170 * (t / t0 ** (-1 / 3) - 1)))       -- nitrogen relaxation frequency
  psat = pres a * 10 ** (-6.8346 * ((t01 / t) ** 1.261) + 4.6151)       -- saturation vapor pressure
  h    = hum a * psat / pres a                                           -- molar concentration of water vapor, as a percentage
  fsq  = f ** 2
  relP = pres a / pr
  t    = tmp a + t0                                                        -- Temp in Kelvin
  t0   = 293.15                                                            -- Kelvin ref temp
  pr   = 101.325                                                           -- reference ambient atmospheric pressure: 101.325 kPa
  t01  = t0 + 0.01                                                         -- triple-point isotherm temp

dlyPhase :: Time -> Freq -> AudioVect -> AudioVect
dlyPhase t f x = cis (f * t * pi * 2) * x

phaseInv :: Bool -> AudioVect -> AudioVect
phaseInv p x = if p then cis pi * x else x

-- Calulation taken from http://www.sengpielaudio.com/calculator-airpressure.htm
speedOfSound :: Atmos -> Double
speedOfSound (Atmos t rh p) = c1 + c2 - c3
 where
  e    = exp 1
  p'   = p * 1000
  tK   = t + 273.15
  enh  = 3.141593e-8 * p' + 1.00062 + t ** 2 * 5.6e-7
  psv1 = tK ** 2 * 1.2378847e-5 - 1.9121316e-2 * tK
  psv2 = 33.93711047 - 6.3431645e3 / tK
  psv  = e ** psv1 * e ** psv2
  xw   = rh * enh * psv / p'
  xc   = 400.0e-6
  c1 =
    0.603055
      *  t
      +  331.5024
      -  t
      ** 2
      *  5.28e-4
      +  (0.1495874 * t + 51.471935 - t ** 2 * 7.82e-4)
      *  xw
  c2 =
    (-1.82e-7 + 3.73e-8 * t - t ** 2 * 2.93e-10)
      * p'
      + (-85.20931 - 0.228525 * t + t ** 2 * 5.91e-5)
      * xc
  c3 =
    xw
      ** 2
      *  2.835149
      -  p'
      ** 2
      *  2.15e-13
      +  xc
      ** 2
      *  29.179762
      +  4.86e-4
      *  xw
      *  p'
      *  xc

dist :: Cord -> Cord -> Float
dist a b = sqrt $ ((fst b - fst a) ^ 2) + ((snd b - snd a) ^ 2)

propTime :: Atmos -> Double -> Time
propTime e d = d / speedOfSound e

splToSp :: Double -> Double
splToSp x = 2.0e-5 * (10 ** (x / 20))

spToSpl :: Double -> Double
spToSpl x = 20 * logBase 10 (x / 2.0e-5)
