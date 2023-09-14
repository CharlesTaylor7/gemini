module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] spec

spec :: Spec Unit
spec =
  describe "Attributes" do
    it "runs in NodeJS" $ pure unit
    it "runs in the browser" $ pure unit
    it "supports streaming reporters" $ pure unit
    it "supports async specs" do
      res <- delay (Milliseconds 100.0) $> "Alligator"
      res `shouldEqual` "Alligator"
    it "is PureScript 0.12.x compatible" $ pure unit
