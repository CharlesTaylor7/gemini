module MarsRover.Api
  ( ParseError
  , fetchPhotos, FetchPhotosParams(..), NasaPhoto(..)
  , fetchRover, roverPayloadParser, roverNames
  , parsePayload
  , module MarsRover.Api.Monad
  )
  where

import Relude

import Data.Traversable (for)

import Data.Vector hiding (length, unsafeHead)
import Optics

import Data.Aeson as Aeson
import Data.Aeson.Types as Aeson

import Rel8

import Colog

import MarsRover.DBSchema
import MarsRover.Env
import MarsRover.Utils
import MarsRover.Api.Monad


newtype ParseError = ParseError String
  deriving stock (Show)
  deriving anyclass (Exception)

defaultQueryParams :: (QueryParam param) => NasaApi param
defaultQueryParams = ask <&> \(NasaApiKey apiKey) -> "api_key" =: apiKey

parsePayload :: (MonadIO m) => (a -> Parser b) -> a -> m b
parsePayload parser = throwLeft . over _Left ParseError . Aeson.parseEither parser

-- | rover names aren't queryable, there's only 3 and they are just listed on the api docs site
roverNames :: [RoverName]
roverNames = [Curiosity, Opportunity, Spirit]

-- | Base url to nasa's api
baseUrl :: Url 'Https
baseUrl = https "api.nasa.gov" /: "mars-photos" /: "api" /: "v1"

-- TODO: generate nasa's api calls from a servant type?
-- | manifest endpoint returns info about a particular rover
fetchRover :: RoverName -> NasaApi (JsonResponse Value)
fetchRover roverName = do
  queryParams <- defaultQueryParams
  req
    GET
    (baseUrl /: "manifests" /: show roverName)
    NoReqBody
    jsonResponse
    queryParams


roverPayloadParser :: Value -> Aeson.Parser (Rover Rel8.Result)
roverPayloadParser = withObject "Rover" $ \v -> do
  rover <- v .: "photo_manifest"
  name <- rover .: "name"
  landingDate <- rover .: "landing_date"
  launchDate <- rover .: "launch_date"
  status <- rover .: "status"
  maxSol <- rover .: "max_sol"
  maxDate <- rover .: "max_date"
  pure $ Rover { id = Id (-1), name, landingDate, launchDate, status, maxSol, maxDate }


data FetchPhotosParams = FetchPhotosParams
  { roverName :: RoverName
  -- , cameraName :: Text
  , sol :: Int64
  , page :: Int64
  }
  deriving stock (Generic, Show)


-- | lists photos in batches of 25 per page
fetchPhotos
  :: FetchPhotosParams
  -> NasaApi (Vector NasaPhoto)
fetchPhotos params@FetchPhotosParams {roverName, sol, page} = do
  runLog $ logDebug $ show params
  queryParams <- defaultQueryParams
  let
    request =
      req
        GET
        (baseUrl /: "rovers" /: show roverName /: "photos")
        NoReqBody
        jsonResponse
        (  queryParams
        <> "sol" =: sol
        <> "page" =: page
        -- <> "camera" =: cameraName
        )
  response <- request
  parsePayload photosPayloadParser $ responseBody response

data NasaPhoto = NasaPhoto
  { roverName :: RoverName
  , cameraName :: Text
  , imgSrc :: Text
  , earthDate :: Day
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

photosPayloadParser :: Value -> Aeson.Parser (Vector NasaPhoto)
photosPayloadParser = withObject "NasaPhotos" $ \obj -> do
  photos <- obj .: "photos"
  for photos $ \photo -> do
    imgSrc <- photo .: "img_src"
    earthDate <- photo .: "earth_date"

    camera <- photo .: "camera"
    cameraName <- camera .: "name"

    rover <- photo .: "rover"
    roverName <- rover .: "name"
    pure $ NasaPhoto { roverName, cameraName, imgSrc, earthDate }
