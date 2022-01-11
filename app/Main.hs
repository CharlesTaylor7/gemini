{-# language OverloadedLabels #-}
{-# options_ghc -Wwarn #-}
module Main where

import           Relude

import           Shpadoinkle                  (JSM)
import           Shpadoinkle.Backend.Snabbdom (runSnabbdom, stage)
import           Shpadoinkle.Html             (addStyle)
import           Shpadoinkle.Run              (liveWithStatic, runJSorWarp, simple)

import           Gemini                       (Store (..), initialState, rootView)

main :: IO ()
main = dev


port :: Int
port = 8080


dev :: IO ()
dev = do
  let initialPage = app initialState
  let staticFolder = "app/static/"
  liveWithStatic port initialPage staticFolder


main' :: IO ()
main' = do
  error "Need to implement static server"
  runJSorWarp port $ app initialState


app :: Store -> JSM ()
app state = do
  addStyle "index.css"
  simple runSnabbdom state rootView stage
