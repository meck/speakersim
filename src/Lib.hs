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
                       , size :: (Double,Double)  -- the Physical size of the speaker
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
attAtmos d f a v = v -- TODO Implement   
 where
  b =
    8.686 * (f ** 2) * (1.84e-11 * ((pres a / pr) ** (-1)) * sqrt (tK / to) + y)
  y =
    (tK / to)
      ** (-5 / 2)
      *  (0.01275 * exp (-2239.1 / tK) * (frO * ((f ** 2) / frO) ** (-1)))
  z   = 0.1068 * exp ((-3352) / tK) * ((frN + (f ** 2) / frN) ** (-1))
  frO = (pres a / pr) * (24 + 4.04e4 * h * ((0.02 + h) / (0.391 + h)))     -- oxygen relaxation frequency
  frN =
    (pres a / pr)
      * (1 / sqrt (tK / to))
      * (9 + 280 * h * exp (-4.170 * ((tK / to) ** (-(1 / 3)) - 1)))       -- nitrogen relaxation frequency
  h    = hum a * (psat / pres a)                                           -- molar concentration of water vapor, as a percentage
  psat = pres a * (10 ** (-6.8346 * ((to1 / tK) ** 1.261) + 4.6151))       -- saturation vapor pressure
  tK   = tmp a - to                                                        -- Temp in Kelvin
  to   = 293.15                                                            -- Kelvin ref temp
  pr   = 101.325                                                           -- reference ambient atmospheric pressure: 101.325 kPa
  to1  = to + 0.01                                                         -- triple-point isotherm temp

dlyPhase :: Time -> Freq -> AudioVect -> AudioVect
dlyPhase t f x = cis (f * t * pi * 2) * x

phaseInv :: Bool -> AudioVect -> AudioVect
phaseInv p x = if p then cis pi * x else x

speedOfSound :: Atmos -> Double
speedOfSound _ = 343.0 -- TODO Make Atomosferic dependent

dist :: Cord -> Cord -> Float
dist a b = sqrt $ ((fst b - fst a) ^ 2) + ((snd b - snd a) ^ 2)

propTime :: Atmos -> Double -> Time
propTime e d = d / speedOfSound e

splToSp :: Double -> Double
splToSp x = 2.0e-5 * (10 ** (x / 20))

spToSpl :: Double -> Double
spToSpl x = 20 * logBase 10 (x / 2.0e-5)
