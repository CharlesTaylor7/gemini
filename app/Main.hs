module Main where

import           Relude

import           Language.Javascript.JSaddle.Warp (run)

import           Shpadoinkle                      (JSM)
import           Shpadoinkle.Backend.ParDiff      (runParDiff, stage)
import           Shpadoinkle.Html                 (addStyle)

import           Gemini                           (Store, initialStore, rootView)


main :: IO ()
main = run port $ app initialStore


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
