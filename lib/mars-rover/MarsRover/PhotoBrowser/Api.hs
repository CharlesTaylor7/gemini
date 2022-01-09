module MarsRover.PhotoBrowser.Api
  ( Rover(..), Camera(..), Photo(..), RoverName
  , MarsRoverApi
  , server
  , getRovers, getPhotos
  , runHandler
  ) where
import           Relude                   hiding (id)

import           Data.Aeson
import           Data.Vector              (Vector)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Optics
import           Polysemy
import           Rel8                     (Day, (==.))
import qualified Rel8
import qualified Rel8.Polysemy            as Rel8
import           Servant
import           Servant.Server           (runHandler)
import           System.IO                (hPutStrLn)
import           Variant

import           MarsRover.DBSchema       (RoverName)
import qualified MarsRover.DBSchema       as Schema
import           MarsRover.Env            (loadEnv)


-- * api
type MarsRoverApi
  =    "rovers"
    :> Get '[JSON] [Rover]
  :<|> "photos"
    :> QueryParam "rover" (Rel8.Id Schema.Rover)
    :> QueryParam "camera" (Rel8.Id Schema.Camera)
    :> Get '[JSON] [Photo]


data Rover = Rover
  { id      :: !Int64
  , name    :: !RoverName
  , cameras :: !(Vector Camera)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, NFData)


data Camera = Camera
  { id   :: !Int64
  , name :: !Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, NFData)


data Photo = Photo
  { imgSrc    :: !Text
  , earthDate :: !Day
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, NFData)


-- * implementation
marsRoverApi :: Proxy MarsRoverApi
marsRoverApi = Proxy

server ::  _
server = getRovers :<|> getPhotos


-- | photos api
getPhotos :: Maybe (Rel8.Id Schema.Rover) -> Maybe (Rel8.Id Schema.Camera) -> Handler [Photo]
getPhotos roverId cameraId = runDBCommandToIO $ do
  query <- Rel8.selectCmd $ do
    photo <- Rel8.each @Schema.Photo

    -- filter by rover
    case roverId of
      Just r -> Rel8.where_ $ Rel8.lit r ==. photo ^. #roverId
      _      -> pass

    -- filter by camera
    case cameraId of
      Just c -> Rel8.where_ $ Rel8.lit c ==. photo ^. #cameraId
      _      -> pass

    pure photo
  pure $ fmap toPhoto query
  where
    toPhoto :: Schema.Photo Rel8.Result -> Photo
    toPhoto = \Schema.Photo { imgSrc, earthDate } -> Photo { imgSrc, earthDate }


-- | rovers api
getRovers :: Handler [Rover]
getRovers = runDBCommandToIO $ do
  query <- Rel8.selectCmd $ do
    rover <- Rel8.each @Schema.Rover
    cameras <- Rel8.many $ do
      camera <- Rel8.each @Schema.Camera
      Rel8.where_ $ rover ^. #id ==. camera ^. #roverId
      pure camera
    pure $ (rover, cameras)

  let
    convertCamera = \Schema.Camera { id = Rel8.Id id, name } ->
      Camera { id, name }

    toResult = \(Schema.Rover { id = Rel8.Id id, name }, cameras) ->
      Rover { id, name, cameras = fromList $ fmap convertCamera cameras }

  pure $ fmap toResult query


runDBCommandToIO :: MonadIO m => Sem _ a -> m a
runDBCommandToIO m = m
    & Rel8.runDBCommand
    & Rel8.runSession
    & Rel8.acquireHasqlConnection
    & errorToVariant @Rel8.QueryError
    & errorToVariant @Rel8.ConnectionError
    & runErrorToExceptionIO @(V '[Rel8.QueryError, Rel8.ConnectionError])
    & runM
