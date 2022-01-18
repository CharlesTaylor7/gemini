module Main where

import           Relude

import           System.Environment          (lookupEnv)

import           Shpadoinkle                 (JSM)
import           Shpadoinkle.Backend.ParDiff (runParDiff, stage)
import           Shpadoinkle.Html            (addStyle)
import           Shpadoinkle.Run             (liveWithStatic, runJSorWarp, simple)

import           Gemini                      (Store, initialStore, rootView)


main :: IO ()
main = do
  port <- getPort
  putStrLn $ "Listening on port " <> show port
  runJSorWarp port $ app Prod initialStore


dev :: IO ()
dev = do
  let initialPage = app Dev initialStore
  let staticFolder = "public/"
  port <- getPort
  liveWithStatic port initialPage staticFolder


data AppEnv
  = Prod
  | Dev
  deriving stock (Eq)


app :: AppEnv -> Store -> JSM ()
app appEnv store = do
  addStyle cssStylePath
  simple runParDiff store rootView stage
    where
      cssStylePath =
        case appEnv of
          Prod -> "/public/styles/index.css"
          Dev  -> "./styles/index.css"


getPort :: IO Int
getPort = do
  maybePort <- lookupEnv "PORT"
  pure $ fromMaybe 8080 $ do
    portString <- maybePort
    readMaybe portString
