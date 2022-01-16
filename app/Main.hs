module Main where

import           Relude

import           Shpadoinkle                 (JSM)
import           Shpadoinkle.Backend.ParDiff (runParDiff, stage)
import           Shpadoinkle.Html            (addStyle)
import           Shpadoinkle.Run             (liveWithStatic, runJSorWarp, simple)

import           Gemini                      (Store, initialStore, rootView)

main :: IO ()
main = dev


port :: Int
port = 8080


dev :: IO ()
dev = do
  let initialPage = app initialStore
  let staticFolder = "app/static/"
  liveWithStatic port initialPage staticFolder


main' :: IO ()
main' = do
  _ <- error "Need to implement static server"
  runJSorWarp port $ app initialStore


app :: Store -> JSM ()
app store = do
  addStyle "index.css"
  simple runParDiff store rootView stage
