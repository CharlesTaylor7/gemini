module Gemini.App
  ( app
  , getEnv
  ) where

import           Relude

import           Shpadoinkle                 (shpadoinkle)
import qualified Shpadoinkle.Backend.ParDiff as Backend
import qualified Shpadoinkle.Html            as Html (addStyle, setTitle)

import           Optics

import           Gemini.Env
import           Gemini.Views.App
import           Gemini.Views.Puzzle
import           Gemini.FFI


app :: Store -> JSM ()
app store = do
  Html.setTitle "Gemini"
  Html.addStyle "/public/styles/index.css"
  territory <- newTVarIO store

  onResize $ do
    domInfo <- loadDomInfo
    atomically $ modifyTVar' territory $ #dom .~ domInfo
  shpadoinkle id Backend.runParDiff territory rootView Backend.stage


getEnv :: Deployment -> IO Env
getEnv deployment = do
  port <- envOptional "PORT" 8000
  commit <- envOptional "COMMIT" "master"
  pure $ Env { deployment, port, commit }
