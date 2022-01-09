module MarsRover.Api.Monad
  ( module Req
  , NasaApi, RateLimited(..), NasaApiKey, runNasaApi
  )
  where

-- relude
import Relude

-- base
import Control.Exception (throwIO)

-- req
import Network.HTTP.Req as Req hiding (Req, runReq)

-- mtl
import Control.Monad.Except

-- optics
import Optics
import Network.HTTP.Optics()

import MarsRover.Env (NasaApiKey(..))


newtype NasaApi a = NasaApi (ReaderT NasaApiKey (ExceptT RateLimited IO) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader NasaApiKey
    , MonadError RateLimited
    )


data RateLimited = RateLimited
  deriving stock (Generic, Show)
  deriving anyclass (Exception)


instance MonadHttp NasaApi where
  handleHttpException e =
    case e ^? statusCode of
      Just 429 -> throwError RateLimited
      _        -> liftIO $ throwIO e
    where
      statusCode :: AffineTraversal' Req.HttpException Int
      statusCode
        = #_VanillaHttpException
        % #_HttpExceptionRequest
        % _2
        % #_StatusCodeException
        % _1
        % #status
        % #code


runNasaApi :: NasaApiKey -> NasaApi a -> IO (Either RateLimited a)
runNasaApi apiKey (NasaApi inner) = runExceptT . flip runReaderT apiKey $ inner
