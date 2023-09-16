module LocationSpec where

import Data.Cyclic
import Data.Foldable
import Data.Gemini
import Data.Gemini.Motions
import Data.Group
import Data.Nat
import Data.Permutation
import Gemini.Prelude

import Data.Array.NonEmpty as NEArray
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.QuickCheck (class Arbitrary, Result(..), (/==), (===))
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotSatisfy, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

newtype AnyLocation = AnyLocation Location

instance Arbitrary AnyLocation where
  arbitrary = AnyLocation <<< indexToLocation <$> Gen.chooseInt 0 53



locationSpec :: Spec Unit
locationSpec = do
  describe "Location" $ do
    it "sibling" $ do
      sibling (location LeftRing 0) `shouldEqual` Nothing
      sibling (location LeftRing 2) `shouldEqual` Just (location CenterRing 16)

    it "sibling is its own (partial) inverse"
      $ quickCheck
      $ \(AnyLocation x) -> case sibling x of
          Nothing -> Success
          Just y -> Just x === sibling y
