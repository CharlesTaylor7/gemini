module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Data.Cyclic
import Data.Nat
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] $ do
  dataCyclicSpec


dataCyclicSpec :: Spec Unit
dataCyclicSpec = do
  describe "Data.Cyclic" $ do
    describe "methods" $ do

      it "creation & destruction" $ do
        let n = unCyclic $ (cyclic 3 :: Cyclic D2)
        n `shouldEqual` 1

      it "compareCyclic" $ do
        ((cyclic @D6 0) `compareCyclic` cyclic 3) `shouldEqual` Opposite
        ((cyclic @D6 0) `compareCyclic` cyclic 6) `shouldEqual` Equal
        ((cyclic @D7 1) `compareCyclic` cyclic 4) `shouldEqual` Precedes
        ((cyclic @D5 1) `compareCyclic` cyclic 4) `shouldEqual` Exceeds

    describe "Ring instance" $ do
      it "addition" $ do
        unCyclic @D10 (cyclic 4 + cyclic 9) `shouldEqual` 3

      it "subtraction" $ do
        unCyclic @D10 (cyclic 4 - cyclic 9) `shouldEqual` 5

      it "multiplication" $ do
        unCyclic @D10 (cyclic 3 * cyclic 7) `shouldEqual` 1


{-
    describe "Group instance" $ do
      it "addition should be the same as the group mappend" $
        2 <> 3 `shouldEqual` (2 <> 3 :: Cyclic D17)

      it "subtraction should be the same as the group subtraction" $
        2 - 3 `shouldEqual` (2 ~~ 3 :: Cyclic D17)

      it "can fold an empty list" $ do
        unCyclic @D3 (fold []) `shouldEqual` 0

      it "can fold a nonempty list" $ do
        unCyclic @D10 (fold [3, 5, 7]) `shouldEqual` 5
        -}
