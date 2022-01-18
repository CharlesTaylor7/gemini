module Main where

import           Relude

import           System.Environment          (lookupEnv)
import           Text.Read                   (readMaybe)

import           Shpadoinkle                 (JSM)
import           Shpadoinkle.Backend.ParDiff (runParDiff, stage)
import           Shpadoinkle.Html            (addStyle)
import           Shpadoinkle.Run             (liveWithStatic, runJSorWarp, simple)

import           Gemini                      (Store, initialStore, rootView)


main :: IO ()
main = dev
{--
do
  port <- getPort
  putStrLn $ "Listening on port " <> show port
  runJSorWarp port $ app initialStore
--}


dev :: IO ()
dev = do
  let initialPage = app initialStore
  let staticFolder = "static/"
  port <- getPort
  liveWithStatic port initialPage staticFolder


app :: Store -> JSM ()
app store = do
  addStyle "./styles/index.css"
  simple runParDiff store rootView stage


getPort :: IO Int
getPort = do
  maybePort <- lookupEnv "PORT"
  pure $ fromMaybe 8080 $ do
    portString <- maybePort
    readMaybe portString
