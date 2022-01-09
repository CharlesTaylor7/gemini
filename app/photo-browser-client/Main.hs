{-# options_ghc -Wwarn #-}
module Main where

import           Relude

import           Control.Exception            (throwIO)
import           Optics                       hiding (simple)

import           Shpadoinkle                  (JSM)
import           Shpadoinkle.Backend.Snabbdom (runSnabbdom, stage)
import           Shpadoinkle.Html             (addStyle)
import           Shpadoinkle.Run              (liveWithStatic, runJSorWarp, simple)

import           MarsRover.Env                (loadEnv)
import           MarsRover.PhotoBrowser.Api   (getRovers, runHandler)
import           PhotoBrowser                 (Store (..), initialState, rootView)


app :: Store -> JSM ()
app state = do
  addStyle "index.css"
  simple runSnabbdom state rootView stage


port :: Int
port = 8080


dev :: IO ()
dev = do
  _ <- loadEnv
  rovers <- runHandler getRovers
  rovers <- either throwIO pure rovers

  let initialPage = initialState & #rovers .~ rovers & app
  let staticFolder = "app/photo-browser-client/static/"
  liveWithStatic port initialPage staticFolder


main :: IO ()
main = do
  error "Need to implement static server"
  runJSorWarp port (app initialState)
