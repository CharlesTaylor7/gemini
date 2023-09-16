module Main where

import Prelude

import Deku.Toplevel (runInBody, runInBody')
import Effect (Effect)
import Gemini.Component.App as App

mainDev :: Effect (Effect Unit)
mainDev = runInBody' App.component

main :: Effect Unit
main = runInBody App.component
