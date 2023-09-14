module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Data.Cyclic
import Data.Nat (class Pos, D2, D18, D54)
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
{-
      it "compareCyclic" $ do
        ((0 :: Cyclic D6) `compareCyclic` 3) `shouldEqual` Opposite
        ((0 :: Cyclic D6) `compareCyclic` 6) `shouldEqual` Equal
        ((1 :: Cyclic D7) `compareCyclic` 4) `shouldEqual` Precedes
        ((1 :: Cyclic D5) `compareCyclic` 4) `shouldEqual` Exceeds

    describe "Num instance" $ do
      it "integer literals" $ do
        unCyclic D@10 3 `shouldEqual` 3
        unCyclic D@10 11 `shouldEqual` 1

      it "addition" $ do
        unCyclic D@10 (4 + 9) `shouldEqual` 3

      it "subtraction" $ do
        unCyclic D@10 (4 - 9) `shouldEqual` 5

      it "multiplication" $ do
        unCyclic D@10 (3 * 7) `shouldEqual` 1

      it "signum" $ do
        -- | always 1
        unCyclic D(signum (-4) :: Cyclic D10) `shouldEqual` 1

    describe "Group instance" $ do
      it "addition should be the same as the group mappend" $
        2 <> 3 `shouldEqual` (2 <> 3 :: Cyclic D17)

      it "subtraction should be the same as the group subtraction" $
        2 - 3 `shouldEqual` (2 ~~ 3 :: Cyclic D17)

      it "can fold an empty list" $ do
        unCyclic D@3 (fold []) `shouldEqual` 0

      it "can fold a nonempty list" $ do
        unCyclic D@10 (fold [3, 5, 7]) `shouldEqual` 5
        -}
