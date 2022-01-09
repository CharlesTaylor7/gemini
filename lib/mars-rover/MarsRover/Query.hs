module MarsRover.Query
  ( insertPhotos
  , jobOrDefault
  ) where

import Relude
import Optics
import Data.Vector hiding (length)

import Polysemy
import Rel8.Polysemy
import Rel8

import MarsRover.Api (NasaPhoto(..))
import MarsRover.DBSchema


insertPhotos :: Member DBCommand r => Vector NasaPhoto -> Sem r ()
insertPhotos photos = do
  -- insert cameras
  insertCmd Insert 
    { into = autoSchema
    , onConflict = DoNothing
    , returning = pure ()
    , rows = do
        -- quote photos from nasa api into Query monad
        (roverName, name) <- Rel8.values $ photos <&> \p -> Rel8.lit (p ^. #roverName, p ^. #cameraName)

        -- join rover by name
        rover <- Rel8.each @Rover
        where_ $ roverName ==. rover ^. #name
        let roverId = rover ^. #id
        let id = Rel8.nextId

        -- construct the camera records for insertion
        pure $ Camera { id, roverId, name }
    }

  -- insert photos
  insertCmd Insert
    { into = autoSchema
    , onConflict = DoNothing
    , returning = pure ()
    , rows = do

        -- quote photos from nasa api into Query monad
        (roverName, cameraName, imgSrc, earthDate) <- Rel8.values $ photos <&> photoToRow

        -- join rover by name
        rover <- Rel8.each @Rover
        where_ $ roverName ==. rover ^. #name

        -- join camera by name
        camera <- Rel8.each @Camera
        where_ $ cameraName ==. camera ^. #name

        -- construct the photo records for insertion
        let id = Rel8.nextId
        let roverId = rover ^. #id
        let cameraId = camera ^. #id
        pure $ Photo { id, roverId, cameraId, imgSrc, earthDate }
    }
  where
    photoToRow :: NasaPhoto -> (Expr RoverName, Expr Text, Expr Text, Expr Day)
    photoToRow p = Rel8.lit
      ( p ^. #roverName
      , p ^. #cameraName
      , p ^. #imgSrc
      , p ^. #earthDate
      )


jobOrDefault :: Rover Expr -> Query (Job Expr)
jobOrDefault rover = do
  j <- Rel8.optional $ do
    job <- Rel8.each
    where_ $ job ^. #roverId ==. rover ^. #id
    pure job

  pure $ maybeTable defaultJob Relude.id j

  where
    defaultJob :: Job Expr
    defaultJob = Job { roverId = rover ^. #id, sol = Rel8.lit 1, page = Rel8.lit 1 }
