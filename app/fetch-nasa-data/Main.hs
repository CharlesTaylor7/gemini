module Main where
import Relude hiding (State, get, runState, evalState)
import Data.Traversable (for)
import Data.Vector hiding (length)

import Optics

import Rel8
import Rel8.Polysemy
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import Polysemy.State
import Polysemy.State.Optics

import Variant

import MarsRover.Api
import MarsRover.DBSchema
import MarsRover.Query
import MarsRover.Env
import MarsRover.Utils
import MarsRover.Log


main :: IO ()
main = do
  env <- loadEnv
  app
    & runLogDebugIO
    & runDBCommand
    & runSession
    & acquireHasqlConnection
    & errorToVariant @QueryError
    & errorToVariant @ConnectionError
    & runErrorStdoutIOFinal @(V '[QueryError, ConnectionError])
    & runInputConst env
    & embedToFinal
    & runFinal


app :: _ => Sem r ()
app = do
  jobs <- upsertRoversAndJobs
  for_ jobs runJob


upsertJob :: Member DBCommand r => Job Result -> Sem r ()
upsertJob job = do
  insertCmd $ Insert
    { into = autoSchema
    , rows = Rel8.values [Rel8.lit job]
    , returning = pure ()
    , onConflict = DoUpdate $ Upsert
      { index = \Job { roverId } -> roverId
      , set = \excluded old -> old { sol = excluded ^. #sol, page = excluded ^. #page } :: Job Expr
      , updateWhere = \_ _ -> true
      }
    }

data JobDone = JobDone
  deriving stock (Show)


loop
  :: forall r.
     Members
      [ Input NasaApiKey
      , Input (Rover Result)
      , State (Job Result)
      , Embed IO
      , Error (V [RateLimited, JobDone])
      , Output (Vector NasaPhoto)
      ] r
  => Sem r ()
loop = do
  apiKey <- input @NasaApiKey
  Rover { name = roverName, maxSol } <- input @(Rover Result)
  Job { sol, page } <- get
  let params = FetchPhotosParams { sol, page, roverName }
  possiblePhotos <- liftIO $ runNasaApi apiKey $ fetchPhotos params
  case possiblePhotos of
    Left RateLimited -> do
      throw (V RateLimited)

    Right photos -> do
      if length photos < 25
      then do
        -- advance to next sol
        #page .= 1
        sol <- #sol <%= (+ 1)
        -- stop because we've exhausted the current job
        when (sol > maxSol) $ throw (V JobDone)
      else do
        -- advance to next page
        #page %= (+ 1)

      -- return photos
      output photos


-- | query the api until rate limited, save the job progress to db, then return all the photos
loadPhotos
  :: forall r.
     Members
      [ Input NasaApiKey
      , Input (Rover Result)
      , State (Job Result)
      , Embed IO
      , Output (Vector NasaPhoto)
      , Log
      , DBCommand
      ] r
  => Sem r ()
loadPhotos = do
  doneEither <- runError $ forever loop
  either (log . show) absurd doneEither


  -- save job progress to db
  job <- get
  log $ "Saving: " <> show job
  upsertJob job



runJob
  :: forall r.
    Members
    [ Embed IO
    , DBCommand
    , Input Env
    , Log
    ] r
  => (Job Result, Rover Result)
  -> Sem r ()
runJob (job, rover) = do
  apiKey <- view #nasaApiKey <$> input
  (photos, _) <- loadPhotos
    & evalState job
    & runInputConst rover
    & runInputConst apiKey
    & runOutputMonoid identity

  insertPhotos photos


upsertRoversAndJobs :: Members '[Embed IO, DBCommand] r => Sem r [(Job Result, Rover Result)]
upsertRoversAndJobs = do
  apiKey <- embed $ loadEnv <&> view #nasaApiKey
  rovers <- embed $ for roverNames $ \roverName -> do
    possibleResponse <- runNasaApi apiKey $ fetchRover roverName
    response <- throwLeft possibleResponse
    let payload = responseBody response
    parsePayload roverPayloadParser payload

  -- insert rovers
  insertCmd $ Insert
    { into = autoSchema
    , onConflict = DoNothing
    , returning = pure ()
    , rows = Rel8.values $
       fmap (\rover -> Rel8.lit rover & #id .~ Rel8.nextId) rovers
    }

  selectCmd $ do
    rover <- Rel8.each
    job <- jobOrDefault rover
    pure (job, rover)
