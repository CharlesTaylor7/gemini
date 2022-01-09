module Main where
import           Relude                     hiding (id)

import           Data.Vector                (Vector)

import           Data.Aeson
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO                  (hPutStrLn)

import           Polysemy

import           Optics
import           Rel8                       (Day, (==.))
import qualified Rel8
import qualified Rel8.Polysemy              as Rel8
import           Variant

import           MarsRover.DBSchema         (RoverName)
import qualified MarsRover.DBSchema         as Schema
import           MarsRover.Env              (loadEnv)
import           MarsRover.PhotoBrowser.Api (MarsRoverApi, server)


main :: IO ()
main = do
  _ <- loadEnv
  let
    port = 8000
    settings = defaultSettings
      & setPort port
      & setBeforeMainLoop (hPutStrLn stderr ("listening on port " <> show port))

  mkApp >>= runSettings settings


-- * app
mkApp :: IO Application
mkApp = pure $ serve (Proxy :: Proxy MarsRoverApi) server
