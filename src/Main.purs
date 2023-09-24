module Main where

import Prelude

import Deku.Toplevel (runInBody)
import Effect (Effect)
import Gemini.Component.App as App

main :: Effect Unit
main = runInBody App.component
