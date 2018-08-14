{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Speakersim
import           Plot
import           Data.Aeson                     ( decode )
import           Options.Applicative
import qualified Data.ByteString.Lazy          as B

data Options = Options {
                        optRes :: Int             -- | Resolution
                       ,optFile :: Maybe FilePath -- | Json file with the world
                       } deriving Show

options :: Parser Options
options =
  Options
    <$> option
          auto
          (short 'r' <> long "resolution" <> metavar "INT" <> value 1 <> help
            "The resolution of the simulation"
          )
    <*> optional
          (strArgument
            (  metavar "FILENAME"
            <> help "A Json file containing the state of the world"
            )
          )

parseOptions :: ParserInfo Options
parseOptions = info
  (options <**> helper)
  (fullDesc <> progDesc "Visualize speaker interaction" <> header "SpeakerPlot")

defaultPlot :: PlotWorld
defaultPlot = PlotWorld
  { frqAtmos = FrqAtmos
    { atmos = Just Atmos {tmp = 20, hum = 0.5, pres = 101.325}
    , freq  = 100.0
    }
  , spkrs    = [idealSpeaker]
  , viewSize = (1000, 1000)
  , viewOrig = (0, 0)
  , pixPerM  = 20
  , showGrid = True
  , scaleMin = 60
  , scaleMax = 100
  }

main :: IO ()
main = do
  Options { optFile, optRes } <- execParser parseOptions

  world                       <- case optFile of
    Nothing -> return defaultPlot
    Just fp -> do
      f <- B.readFile fp
      case decode f of
        Nothing -> error $ "Error reading file: " ++ show fp
        Just w' -> return w'
  showPlot "Plot" (100, 100) optRes world
