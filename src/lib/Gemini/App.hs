module Gemini.App
  ( app
  , getEnv
  ) where

import           Relude

import           Shpadoinkle                 (JSM)
import           Shpadoinkle.Backend.ParDiff (runParDiff, stage)
import           Shpadoinkle.Html            (addStyle, setTitle)
import           Shpadoinkle.Run             (simple)

import           Gemini.Env
import           Gemini.Views.App


app :: Store -> JSM ()
app store = do
  setTitle "Gemini"
  addStyle "/public/styles/index.css"
  simple runParDiff store rootView stage


getEnv :: Deployment -> IO Env
getEnv deployment = do
  port <- envOptional "PORT" 8000
  commit <- envOptional "COMMIT" "master"
  pure $ Env { deployment, port, commit }
