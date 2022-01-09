{-# language StrictData #-}
module MarsRover.DBSchema
  ( Rover(..), RoverName(..)
  , Photo(..)
  , Job(..)
  , Camera(..)
  )
  where
import           Data.Aeson (FromJSON, ToJSON)
import           Rel8
import           Relude


-- | Rover
data Rover f = Rover
  { id          :: Column f (Id Rover)
  , name        :: Column f RoverName
  , landingDate :: Column f Day
  , launchDate  :: Column f Day
  , status      :: Column f Text
  , maxSol      :: Column f Int64
  , maxDate     :: Column f Day
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able, AutoSchema)

deriving stock instance f ~ Rel8.Result => Show (Rover f)

-- | There are only 3 mars rovers, and they aren't queryable by api, just listed on the website.
-- | So we're defining an enum for them
data RoverName
  = Curiosity
  | Opportunity
  | Spirit
  deriving stock (Generic, Read, Show, Bounded, Relude.Enum, Eq)
  deriving DBType via ReadShow RoverName
  deriving anyclass (DBEq, FromJSON, ToJSON, NFData)


-- | Photo
data Photo f = Photo
  { id        :: Column f (Id Photo)
  , roverId   :: Column f (Id Rover)
  , cameraId  :: Column f (Id Camera)
  , imgSrc    :: Column f Text
  , earthDate :: Column f Day
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able, AutoSchema)

deriving stock instance f ~ Rel8.Result => Show (Photo f)


-- | Job
data Job f = Job
  { roverId :: Column f (Id Rover)
  , sol     :: Column f Int64
  , page    :: Column f Int64
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able, AutoSchema)

deriving stock instance f ~ Rel8.Result => Show (Job f)


-- | Camera (view)
data Camera f = Camera
  { id      :: Column f (Id Camera)
  , roverId :: Column f (Id Rover)
  , name    :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able, AutoSchema)

deriving stock instance f ~ Rel8.Result => Show (Camera f)
