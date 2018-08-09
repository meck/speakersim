{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Speakersim
  ( Cord
  , Time
  , Freq
  , AudioVect
  , Atmos(..)
  , FrqAtmos(..)
  , Speaker(..)
  , idealSpeaker
  , ReaderResp
  , totalAtPoint
  , audioVecToSpl
  , splToSp
  , spToSpl
  )
where

import           Control.Applicative            ( liftA2 )
import           Data.Complex                   ( Complex(..)
                                                , magnitude
                                                , phase
                                                , cis
                                                , mkPolar
                                                )
import           Control.Monad.Reader
import           Data.Aeson
import           GHC.Generics

-- | An (x,y) cordinate in meters
type Cord = (Float, Float)

-- | Time in sec
type Time = Double

-- | Frequency in Hz
type Freq = Double

-- | Vector with magnitude and phase
type AudioVect = Complex Double

-- | Atmospheric conditions
data Atmos = Atmos { tmp :: Double  -- ^ Temperature in C
                   , hum :: Double  -- ^ Humidity in %
                   , pres :: Double -- ^ Ambient atmospheric pressue in kPa
                   } deriving (Generic, Show)

instance FromJSON Atmos
instance ToJSON Atmos

-- | The frequency of interest
-- with optional atmospheric conditions
data FrqAtmos = FrqAtmos { atmos :: Maybe Atmos,
                           freq :: Freq
                         } deriving (Generic, Show)

instance FromJSON FrqAtmos
instance ToJSON FrqAtmos

-- | Response awaiting an environment
type ReaderResp = Reader FrqAtmos AudioVect

-- | A Speaker
data Speaker = Speaker { pos :: Cord              -- ^ Physical placment of a speaker in meters
                       , level :: Double          -- ^ Level of the speaker in dB (<= 0)
                       , polInv :: Bool           -- ^ Polarity
                       , dly :: Time              -- ^ Processing delay in seconds
                       , res :: Freq -> AudioVect -- ^ Pa measured @ 1 meter at f
                       , size :: (Float,Float)    -- ^ Physical size of the speaker in meters
                       }

-- | A speaker with ideal frequency responce
idealSpeaker :: Speaker
idealSpeaker = Speaker
  { pos    = (0, 0)
  , level  = 0
  , dly    = 0
  , polInv = False
  , res    = const (1 :+ 0)
  , size   = (1, 1)
  }

instance Show Speaker where
  show s = unlines [ "pos: " <> show (pos s),
                     "level: " <> show (level s),
                     "polInv: " <> show (polInv s),
                     "size: " <> show (size s),
                     "dly: " <> show (dly s)]

instance ToJSON Speaker where
  toJSON s = object ["pos" .= pos s, "level" .= level s, "polInv" .= polInv s, "dly" .= dly s, "size" .= size s]

instance FromJSON Speaker where
  parseJSON = withObject "Speaker" $ \s -> Speaker
    <$> s .: "pos"
    <*> s .: "level"
    <*> s .: "polInv"
    <*> s .: "dly"
    <*> pure (const (1:+0))
    <*> s .: "size"


-- | Total Response at cord from multiple speakers
totalAtPoint :: Cord -> [Speaker] -> ReaderResp
totalAtPoint c ss = foldr (liftA2 (+)) (pure (0 :+ 0)) $ respAtPoint c <$> ss

-- | Respone of a single speaker at cord
respAtPoint :: Cord -> Speaker -> ReaderResp
respAtPoint p s = do
  (FrqAtmos a f) <- ask
  let d = realToFrac $ dist p $ pos s
      t = propTime a (realToFrac d) + dly s
      i = polInv s
  return $ attAtmos d f a $ attDist d $ phaseInv i $ dlyPhase t f $ res s f

-- | Utillity for converting a vector of Pa to SPL
audioVecToSpl :: AudioVect -> Double
audioVecToSpl = spToSpl . magnitude

-- | dBSPL to Pa
splToSp :: Double -> Double
splToSp x = 2.0e-5 * (10 ** (x / 20))

-- | Pa to dBSPL
spToSpl :: Double -> Double
spToSpl x = 20 * logBase 10 (x / 2.0e-5)

attDist :: Double -> AudioVect -> AudioVect
attDist 0 x = x
attDist d x = mkPolar (1 / d) 0 * x

-- Calulation taken from http://www.sengpielaudio.com/calculator-air.htm
attAtmos :: Double -> Freq -> Maybe Atmos -> AudioVect -> AudioVect
attAtmos _ _ Nothing  v = v
attAtmos d f (Just a) v = mkPolar (magnitude v / fact) (phase v)
 where
  t    = tmp a + 273.15                                                        -- Temp in K
  p    = pres a * 1000 / 101325                                                -- Relative pressure
  cHum = 4.6151 - 6.8346 * (273.15 / t) ** 1.261
  h    = (hum a * 100) * 10 ** cHum * p
  tr   = t / (273.15 + 20)                                                     -- Relative temp to 20C
  frO  = p * (24 + 4.04e4 * h * (0.02 + h) / (0.391 + h))                      -- Oxygen relaxation frequency
  frN =
    p * ((tr ** (-0.5)) * (9 + 280 * h * exp (-4.17 * (tr ** (-1 / 3) - 1))))  -- Nitrogen relaxation frequency
  deltaM =
    8.686
      * f
      * f
      * ( 1.84e-11
        * (1 / p)
        * sqrt tr
        + (tr ** (-2.5))
        * ( 0.01275
          * (exp (-2239.1 / t) / (frO + f * f / frO))
          + 0.1068
          * (exp (-3352 / t) / (frN + f * f / frN))
          )
        )                                                                      -- The dampening in dB/m
  fact = 10 ** (deltaM * d / 20)

dlyPhase :: Time -> Freq -> AudioVect -> AudioVect
dlyPhase t f x = cis (f * t * pi * 2) * x

phaseInv :: Bool -> AudioVect -> AudioVect
phaseInv p x = if p then cis pi * x else x

-- Calulation taken from http://www.sengpielaudio.com/calculator-airpressure.htm
speedOfSound :: Maybe Atmos -> Double
speedOfSound Nothing               = 343.0               -- Default value
speedOfSound (Just (Atmos t rh p)) = c1 + c2 - c3
 where
  e    = exp 1
  p'   = p * 1000                                        -- ambient pressue in pA
  tK   = t + 273.15                                      -- Temp in Kelvin
  enh  = 3.141593e-8 * p' + 1.00062 + (t ** 2) * 5.6e-7  -- Moleculear concentration of water wapor
  psv1 = (tK ** 2) * 1.2378847e-5 - 1.9121316e-2 * tK
  psv2 = 33.93711047 - 6.3431645e3 / tK
  psv  = (e ** psv1) * (e ** psv2)
  xw   = rh * enh * psv / p'                             -- Mole fraction of water
  xc   = 400.0e-6                                        -- Mole fraction of CO2
  c1 =
    0.603055
      * t
      + 331.5024
      - (t ** 2)
      * 5.28e-4
      + (0.1495874 * t + 51.471935 - (t ** 2) * 7.82e-4)
      * xw
  c2 =
    (-1.82e-7 + 3.73e-8 * t - (t ** 2) * 2.93e-10)
      * p'
      + (-85.20931 - 0.228525 * t + (t ** 2) * 5.91e-5)
      * xc
  c3 =
    (xw ** 2)
      * 2.835149
      - (p' ** 2)
      * 2.15e-13
      + (xc ** 2)
      * 29.179762
      + 4.86e-4
      * xw
      * p'
      * xc

dist :: Cord -> Cord -> Float
dist a b = sqrt $ ((fst b - fst a) ** 2) + ((snd b - snd a) ** 2)

propTime :: Maybe Atmos -> Double -> Time
propTime a d = d / speedOfSound a
