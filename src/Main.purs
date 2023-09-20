module Main where

import Data.Foldable
import Data.Location
import Prelude

import Deku.Toplevel (runInBody, runInBody')
import Effect (Effect)
import Effect.Console as Console
import Gemini.Component.App as App

mainDev :: Effect (Effect Unit)
mainDev = runInBody' App.component

main :: Effect Unit
main = do
  Console.log $ show $
    ambiguousLocations <#> _.alternate >>> locationToIndex

  runInBody App.component
