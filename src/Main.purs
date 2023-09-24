module Main where

import Data.Foldable
import Data.Location
import Data.Permutation
import Debug
import Prelude
import Test.Gemini.Gen

import Deku.Toplevel (runInBody, runInBody')
import Effect (Effect)
import Effect.Console as Console
import Gemini.Component.App as App
import Test.QuickCheck.Gen as Gen

mainDev :: Effect (Effect Unit)
mainDev = runInBody' App.component

main :: Effect Unit
main = do
  sample <- Gen.randomSample $
    arbitrary <#> \(AnyPermutation p) -> (p :: _ 2)
  Console.log $ show sample

  runInBody App.component
