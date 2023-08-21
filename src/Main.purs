module Main where

import Prelude
import Effect (Effect)

import Deku.Toplevel (runInBody, runInBody')

import Gemini.Component.App as App


mainDev :: Effect (Effect Unit)
mainDev = runInBody' App.component


main :: Effect Unit
main = runInBody App.component
