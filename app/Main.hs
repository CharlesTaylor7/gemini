module Main where

import           Relude

import           Shpadoinkle                 (JSM)
import           Shpadoinkle.Backend.ParDiff (runParDiff, stage)
import           Shpadoinkle.Html            (addStyle)
import           Shpadoinkle.Run             (liveWithStatic, runJSorWarp, simple)

import           Gemini                      (Store, initialStore, rootView)

main :: IO ()
main = runJSorWarp port $ app initialStore


port :: Int
port = 8080


dev :: IO ()
dev = do
  let initialPage = app initialStore
  let staticFolder = "static/"
  liveWithStatic port initialPage staticFolder



app :: Store -> JSM ()
app store = do
  addStyle "./public/index.css"
  simple runParDiff store rootView stage
