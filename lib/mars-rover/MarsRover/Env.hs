module MarsRover.Env
    ( loadEnv
    , Env(..), NasaApiKey(..)
    , runLog
    ) where

-- relude
import Relude

-- base
import System.Environment (setEnv, getEnv)

-- text
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Colog
runLog :: MonadIO m => LoggerT Message m a -> m a
runLog = usingLoggerT action
  where action = cmap fmtMessage logTextStdout


data Env = Env
  { nasaApiKey :: NasaApiKey
  }
  deriving stock (Generic)

-- | Note: Do not give this a show instance
newtype NasaApiKey = NasaApiKey Text


-- | Parses the .env file and sets variables in the environment.
-- | Parses a subset of those variables into the app Env data type.
-- | For example, we need postgres env variables in the system environment, but not in the application environment.
loadEnv :: IO Env
loadEnv = do
  contents <- T.readFile ".env"
  for_ (T.lines contents) $ \line ->
    let
      (envName, rest) = T.break (== '=') line
      envValue = T.tail rest
    in
      setEnv (T.unpack envName) (T.unpack envValue)

  nasaApiKey <- NasaApiKey . T.pack <$> getEnv "NASA_API_KEY"
  pure $ Env { nasaApiKey }
